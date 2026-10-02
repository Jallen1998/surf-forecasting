# fetch_forecast.R
# Pulls marine (swell/wave) and weather (wind) forecast data per spot,
# hitting the Open-Meteo HTTP API directly rather than relying on the
# `openmeteo` package's default "best_match" model selection — that
# blend has a documented failure mode where it returns HTTP 200 with
# every marine variable silently NULL for some near-coastal points
# (see github.com/open-meteo/open-meteo/issues/1364). We pin an
# explicit model and verify real data came back before trusting it.
#
# Install once: install.packages(c("httr", "jsonlite", "dplyr", "yaml", "purrr"))

library(httr)
library(jsonlite)
library(dplyr)
library(yaml)
library(purrr)

MARINE_URL <- "https://marine-api.open-meteo.com/v1/marine"
WEATHER_URL <- "https://api.open-meteo.com/v1/forecast"

# Priority order: try the higher-resolution regional model first, fall
# back to the coarser global model only if the regional one comes back
# empty for this specific point. Confirmed identifiers as of this build:
# dwd_ewam = DWD's European wave model, ~5km, North Sea + Baltic coverage.
# ncep_gfswave025 = NOAA's global wave model, ~25km, confirmed working
# at a comparable near-coastal European point where dwd_ewam/best_match
# returned nulls.
MARINE_MODEL_PRIORITY <- c("dwd_ewam", "ncep_gfswave025")
# Wind mirrors the marine blend: ICON-EU (EWAM's own forcing, ~7km) for
# as long as it runs (~5 days), then GFS, which is what forces GFS Wave,
# so beyond day 5 the wave and wind numbers come from the same model.
# Without the fallback every block after ~day 5 had wave data but NA
# wind and scored NA, which silently capped the horizon at 5 days.
WIND_MODEL_PRIORITY <- c("dwd_icon_eu", "gfs_seamless")

# ---- Config ------------------------------------------------------------
load_spots <- function(path = "config/spots.yaml") {
  cfg <- read_yaml(path)

  pending <- names(cfg$spots)[map_lgl(cfg$spots, is.null)]
  if (length(pending) > 0) {
    message(
      "Skipping pending spots (no coordinates yet): ",
      paste(pending, collapse = ", ")
    )
  }

  cfg$spots |>
    keep(~ !is.null(.x)) |>
    imap(function(spot_cfg, spot_name) {
      tibble(
        spot = spot_name,
        lat = spot_cfg$lat,
        lon = spot_cfg$lon,
        tier = spot_cfg$tier,
        facing_deg = spot_cfg$facing_deg,
        offshore_arc_min = spot_cfg$offshore_arc[[1]],
        offshore_arc_max = spot_cfg$offshore_arc[[2]],
        # Optional per-spot direction tolerance (NA = use tiers.yaml default)
        exposure_full_deg = spot_cfg$exposure_full_deg %||% NA_real_,
        exposure_zero_deg = spot_cfg$exposure_zero_deg %||% NA_real_
      )
    }) |>
    bind_rows()
}

# ---- Low-level API call, one model, one spot -----------------------------
.call_openmeteo <- function(
  base_url,
  lat,
  lon,
  hourly_vars,
  model,
  extra_params = list(),
  days_ahead = 10
) {
  params <- c(
    list(
      latitude = lat,
      longitude = lon,
      hourly = paste(hourly_vars, collapse = ","),
      models = model,
      forecast_days = min(days_ahead, 16),
      # GMT, not "auto": "auto" returns LOCAL clock times, which the
      # parsers below label as UTC (silently 1-2h wrong, and DST-change
      # days can repeat/skip an hour). Keep everything true-UTC
      # internally; convert to Europe/Copenhagen only for display.
      timezone = "GMT"
    ),
    extra_params
  )
  resp <- GET(base_url, query = params)
  if (status_code(resp) != 200) {
    warning(sprintf(
      "Open-Meteo request failed (%s) for model %s at %.3f,%.3f",
      status_code(resp),
      model,
      lat,
      lon
    ))
    return(NULL)
  }
  fromJSON(content(resp, "text", encoding = "UTF-8"))
}

# Returns TRUE if the response actually has non-null data in its first
# requested hourly variable — catches the silent-null failure mode.
.has_real_data <- function(parsed, check_var) {
  !is.null(parsed$hourly) &&
    !is.null(parsed$hourly[[check_var]]) &&
    any(!is.na(parsed$hourly[[check_var]]))
}

# ---- Marine pull, with fallback across models -----------------------------
.pull_one_marine_model <- function(lat, lon, hourly_vars, model, days_ahead) {
  parsed <- .call_openmeteo(
    MARINE_URL,
    lat,
    lon,
    hourly_vars,
    model,
    days_ahead = days_ahead
  )
  if (is.null(parsed) || !.has_real_data(parsed, "wave_height")) {
    return(NULL)
  }

  as_tibble(parsed$hourly) |>
    mutate(
      datetime = as.POSIXct(time, format = "%Y-%m-%dT%H:%M", tz = "UTC"),
      marine_model = model
    ) |>
    select(-time) |>
    filter(!is.na(wave_height))
}

# Blends models rather than picking one: EWAM's real near-term coverage
# (confirmed ~3.4 days despite an 8-day documented spec — likely a limit
# in what Open-Meteo stores/serves for this model, not a physical limit)
# is used where it exists; the global GFS Wave fallback fills whatever
# hours beyond that, up to days_ahead. marine_model is now per-ROW, not
# per-call, so scored output shows exactly which model backs each hour.
fetch_marine <- function(lat, lon, days_ahead = 10) {
  hourly_vars <- c(
    "wave_height",
    "wave_direction",
    "wave_period",
    "wind_wave_height",
    "wind_wave_direction",
    "wind_wave_period",
    "swell_wave_height",
    "swell_wave_direction",
    "swell_wave_period"
  )

  primary <- .pull_one_marine_model(
    lat,
    lon,
    hourly_vars,
    MARINE_MODEL_PRIORITY[1],
    days_ahead
  )

  if (is.null(primary)) {
    message(sprintf(
      "Primary marine model '%s' returned no data at %.3f,%.3f — using fallback only.",
      MARINE_MODEL_PRIORITY[1],
      lat,
      lon
    ))
    primary <- tibble()
  } else if (nrow(primary) < days_ahead * 24) {
    message(sprintf(
      "Marine model '%s' at %.3f,%.3f covers %.1f days — filling the rest with '%s'.",
      MARINE_MODEL_PRIORITY[1],
      lat,
      lon,
      nrow(primary) / 24,
      MARINE_MODEL_PRIORITY[2]
    ))
  } else {
    return(primary) # full coverage from the primary model, no fallback needed
  }

  fallback <- .pull_one_marine_model(
    lat,
    lon,
    hourly_vars,
    MARINE_MODEL_PRIORITY[2],
    days_ahead
  )
  if (is.null(fallback)) {
    if (nrow(primary) == 0) {
      stop(sprintf("No marine model returned data for %.3f,%.3f.", lat, lon))
    }
    return(primary) # partial coverage is still better than erroring out
  }

  last_primary_hour <- if (nrow(primary) > 0) {
    max(primary$datetime)
  } else {
    as.POSIXct(-Inf, tz = "UTC")
  }
  fallback_tail <- fallback |> filter(datetime > last_primary_hour)

  bind_rows(primary, fallback_tail) |> arrange(datetime)
}

# ---- Wind pull, with fallback across models --------------------------------
.pull_one_wind_model <- function(lat, lon, model, days_ahead) {
  hourly_vars <- c("wind_speed_10m", "wind_direction_10m", "wind_gusts_10m")
  parsed <- .call_openmeteo(
    WEATHER_URL,
    lat,
    lon,
    hourly_vars,
    model,
    extra_params = list(wind_speed_unit = "ms"),
    days_ahead = days_ahead
  )
  if (is.null(parsed) || !.has_real_data(parsed, "wind_speed_10m")) {
    return(NULL)
  }
  as_tibble(parsed$hourly) |>
    mutate(
      datetime = as.POSIXct(time, format = "%Y-%m-%dT%H:%M", tz = "UTC"),
      wind_model = model
    ) |>
    select(-time) |>
    # A model past its horizon returns rows of NA rather than no rows.
    filter(!is.na(wind_speed_10m), !is.na(wind_direction_10m))
}

fetch_wind <- function(lat, lon, days_ahead = 10) {
  primary <- .pull_one_wind_model(lat, lon, WIND_MODEL_PRIORITY[1], days_ahead)

  if (is.null(primary)) {
    message(sprintf(
      "Primary wind model '%s' returned no data at %.3f,%.3f — using fallback only.",
      WIND_MODEL_PRIORITY[1],
      lat,
      lon
    ))
    primary <- tibble()
  } else if (nrow(primary) >= days_ahead * 24) {
    return(primary)
  }

  fallback <- .pull_one_wind_model(lat, lon, WIND_MODEL_PRIORITY[2], days_ahead)
  if (is.null(fallback)) {
    if (nrow(primary) == 0) {
      stop(sprintf("No wind model returned data for %.3f,%.3f.", lat, lon))
    }
    warning(sprintf(
      "Fallback wind model '%s' failed at %.3f,%.3f — horizon limited to %.1f days.",
      WIND_MODEL_PRIORITY[2],
      lat,
      lon,
      nrow(primary) / 24
    ))
    return(primary)
  }

  last_primary_hour <- if (nrow(primary) > 0) {
    max(primary$datetime)
  } else {
    as.POSIXct(-Inf, tz = "UTC")
  }
  bind_rows(primary, fallback |> filter(datetime > last_primary_hour)) |>
    arrange(datetime)
}

# ---- Combine for one spot / all spots --------------------------------------
fetch_spot_forecast <- function(spot_row, days_ahead = 10) {
  marine <- fetch_marine(spot_row$lat, spot_row$lon, days_ahead)
  wind <- fetch_wind(spot_row$lat, spot_row$lon, days_ahead)

  marine |>
    inner_join(wind, by = "datetime") |>
    mutate(
      spot = spot_row$spot,
      tier = spot_row$tier,
      facing_deg = spot_row$facing_deg,
      offshore_arc_min = spot_row$offshore_arc_min,
      offshore_arc_max = spot_row$offshore_arc_max,
      exposure_full_deg = spot_row$exposure_full_deg,
      exposure_zero_deg = spot_row$exposure_zero_deg
    ) |>
    relocate(spot, tier, datetime)
}

fetch_all_spots <- function(
  spots_path = "config/spots.yaml",
  days_ahead = 10,
  spots = load_spots(spots_path) # pass an already-loaded table to skip re-reading
) {
  spots |>
    split(seq_len(nrow(spots))) |>
    map(fetch_spot_forecast, days_ahead = days_ahead) |>
    bind_rows()
}

# ---- Manual test run -------------------------------------------------------
# Run this first against every spot before wiring up anything downstream.
# For each spot, confirm marine_model actually used matches what you
# expect (dwd_ewam, not the ncep fallback) — if a spot is consistently
# falling back, that's worth knowing and investigating, not ignoring.
if (sys.nframe() == 0) {
  forecast <- fetch_all_spots()
  cat("Model used per spot:\n")
  print(forecast |> count(spot, marine_model, wind_model), n = Inf)
  print(head(forecast, 10))
}

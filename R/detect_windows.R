# detect_windows.R
# Turns hourly scored blocks into "windows": runs of Good+ surf in
# daylight at one spot. Pure function of the current run's data. It has
# no memory of previous runs. Matching windows across runs (new /
# confirmed / changed / cancelled) is the job of the tracking step,
# which consumes this output.
#
# Rules (parameters in config/windows.yaml):
#   1. Only daylight blocks count. Night breaks a window, so a Good
#      evening and a Good next morning are two windows, not one.
#   2. Hysteresis: a window needs at least one block >= Good to exist,
#      and stays open while score >= Good - hold_margin. Stops a score
#      wobbling around 6.0 from splitting one session into three.
#   3. Dips below the hold level lasting <= max_gap_hours are bridged.
#   4. The window is trimmed to its first/last Good+ block (hold-level
#      shoulders don't stretch it), then dropped if shorter than
#      min_duration_hours.
#
# Requires: R/daylight.R sourced first.
# Install once: install.packages(c("dplyr", "yaml", "purrr", "tibble"))

library(dplyr)
library(yaml)
library(purrr)

load_window_cfg <- function(path = "config/windows.yaml") {
  read_yaml(path)$detection
}

# ---- Assemble the block table ---------------------------------------------
# score_block.R returns score + category + limiting_factor only, so the
# raw swell/wind numbers and marine_model (needed for messages and for
# confidence) are joined back on from the forecast. spots gives lat/lon.
assemble_blocks <- function(forecast, scored, spots) {
  dups <- forecast |> count(spot, datetime) |> filter(n > 1)
  if (nrow(dups) > 0) {
    stop(
      "Duplicate spot/datetime rows in forecast (",
      nrow(dups),
      "), e.g. ",
      dups$spot[1],
      " ",
      format(dups$datetime[1]),
      ". Usually a timezone/DST problem: check fetch_forecast.R requests timezone = 'GMT'."
    )
  }

  raw_cols <- c(
    "marine_model",
    "wind_model",
    "wave_height",
    "wave_period",
    "wave_direction",
    "swell_wave_height",
    "swell_wave_period",
    "swell_wave_direction",
    "wind_speed_10m",
    "wind_direction_10m",
    "wind_gusts_10m"
  )

  scored |>
    ungroup() |>
    left_join(
      forecast |> select(spot, datetime, any_of(raw_cols)),
      by = c("spot", "datetime")
    ) |>
    left_join(spots |> select(spot, lat, lon), by = "spot") |>
    arrange(spot, datetime)
}

add_daylight <- function(blocks, cfg) {
  blocks |>
    mutate(
      is_daylight = is_daylight(
        datetime,
        lat,
        lon,
        light = cfg$light,
        pad_min = cfg$daylight_pad_min
      )
    )
}

# ---- Core detection for ONE spot -------------------------------------------
# d: that spot's full hourly series (night included), sorted, with
# is_daylight. Returns a list of index pairs (start_i, end_i) into d.
.find_spans <- function(d, good_cutoff, cfg) {
  hold <- good_cutoff - cfg$hold_margin
  ok <- d$is_daylight & !is.na(d$score)

  # Daylight segments: contiguous, hourly-consecutive usable rows. Any
  # night hour or missing hour ends a segment.
  hour_step <- c(NA, diff(as.numeric(d$datetime)) / 3600)
  new_seg <- !ok | is.na(hour_step) | hour_step != 1 | !c(FALSE, head(ok, -1))
  seg_id <- cumsum(new_seg)
  seg_id[!ok] <- NA

  spans <- list()
  for (s in unique(na.omit(seg_id))) {
    idx <- which(seg_id == s)
    warm <- idx[d$score[idx] >= hold]
    if (length(warm) == 0) next

    # Cluster warm hours, bridging gaps of <= max_gap_hours.
    cluster <- cumsum(c(TRUE, diff(warm) - 1 > cfg$max_gap_hours))
    for (k in unique(cluster)) {
      span <- range(warm[cluster == k])
      span_idx <- span[1]:span[2]
      hot <- span_idx[d$score[span_idx] >= good_cutoff]
      if (length(hot) == 0) next # hold-level only, never reached Good
      if (max(hot) - min(hot) + 1 < cfg$min_duration_hours) next
      spans[[length(spans) + 1]] <- c(min(hot), max(hot))
    }
  }
  spans
}

# What sits just outside the window: darkness, end of forecast, or the
# limiting_factor of the neighbouring block (which is what stopped it).
.edge_reason <- function(d, i) {
  if (i < 1 || i > nrow(d)) {
    return("forecast_edge")
  }
  if (!d$is_daylight[i]) {
    return("darkness")
  }
  lf <- d$limiting_factor[i]
  if (is.na(lf) || lf == "none") "score_dip" else lf
}

.mode_limit <- function(x) {
  x <- x[!is.na(x) & x != "none"]
  if (length(x) == 0) {
    return("none")
  }
  names(sort(table(x), decreasing = TRUE))[1]
}

.summarise_span <- function(d, span, good_cutoff, now) {
  w <- d[span[1]:span[2], ]
  p <- which.max(w$score)
  start <- w$datetime[1]
  end <- w$datetime[nrow(w)]

  tibble::tibble(
    spot = w$spot[1],
    tier = w$tier[1],
    start = start,
    end = end, # last block's hour, inclusive
    n_hours = nrow(w),
    n_good_hours = sum(w$score >= good_cutoff),
    peak_time = w$datetime[p],
    peak_score = w$score[p],
    peak_category = w$category[p],
    mean_score = round(mean(w$score), 1),
    wave_height = w$wave_height[p],
    wave_period = w$wave_period[p],
    swell_height = w$swell_wave_height[p],
    swell_period = w$swell_wave_period[p],
    swell_dir = w$swell_wave_direction[p],
    wind_speed = w$wind_speed_10m[p],
    wind_dir = w$wind_direction_10m[p],
    wind_category = w$wind_category[p],
    block_type = w$block_type[p],
    dominant_limit = .mode_limit(w$limiting_factor),
    before_start = .edge_reason(d, span[1] - 1),
    after_end = .edge_reason(d, span[2] + 1),
    ewam_share = mean(w$marine_model == "dwd_ewam", na.rm = TRUE),
    status = dplyr::case_when(
      end + 3600 < now ~ "past",
      start <= now ~ "ongoing",
      TRUE ~ "upcoming"
    ),
    lead_hours = round(as.numeric(difftime(start, now, units = "hours")), 1)
  )
}

# ---- Public entry point ------------------------------------------------------
# blocks: output of assemble_blocks() |> add_daylight().
# good_cutoff: tiers_cfg$category_cutoffs$good.
# now: injectable for testing and for reproducible re-runs.
detect_windows <- function(blocks, good_cutoff, cfg, now = Sys.time()) {
  needed <- c(
    "spot",
    "datetime",
    "score",
    "category",
    "limiting_factor",
    "is_daylight"
  )
  missing <- setdiff(needed, names(blocks))
  if (length(missing) > 0) {
    stop("detect_windows: blocks missing columns: ", paste(missing, collapse = ", "))
  }

  blocks |>
    arrange(spot, datetime) |>
    split(~spot) |>
    map(function(d) {
      d <- as.data.frame(d)
      spans <- .find_spans(d, good_cutoff, cfg)
      map(spans, ~ .summarise_span(d, .x, good_cutoff, now)) |> list_rbind()
    }) |>
    list_rbind() |>
    (\(x) if (nrow(x) == 0) .empty_windows() else arrange(x, start, spot))()
}

# Zero-row result with the full column set. A flat week produces no
# windows, and downstream code (tracking, the page) must still find every
# column; list_rbind() of nothing returns a table with NO columns.
.empty_windows <- function() {
  ts <- as.POSIXct(character(), tz = "UTC")
  tibble::tibble(
    spot = character(), tier = character(), start = ts, end = ts,
    n_hours = integer(), n_good_hours = integer(), peak_time = ts,
    peak_score = numeric(), peak_category = character(), mean_score = numeric(),
    wave_height = numeric(), wave_period = numeric(), swell_height = numeric(),
    swell_period = numeric(), swell_dir = numeric(), wind_speed = numeric(),
    wind_dir = numeric(), wind_category = character(), block_type = character(),
    dominant_limit = character(), before_start = character(), after_end = character(),
    ewam_share = numeric(), status = character(), lead_hours = numeric()
  )
}

# ---- Human-readable check (local time) --------------------------------------
print_windows <- function(windows, tz = "Europe/Copenhagen") {
  if (nrow(windows) == 0) {
    cat("No Good+ daylight windows in this forecast.\n")
    return(invisible(windows))
  }
  for (i in seq_len(nrow(windows))) {
    w <- windows[i, ]
    cat(sprintf(
      "%-16s %s %s-%s (%dh, %d Good+)  peak %.1f %s @ %s  | %.1fm @ %.0fs, wind %.0f m/s %s | limit: %s | before: %s, after: %s | EWAM %.0f%% | %s\n",
      w$spot,
      format(w$start, "%a %d %b", tz = tz),
      format(w$start, "%H:%M", tz = tz),
      format(w$end, "%H:%M", tz = tz),
      w$n_hours,
      w$n_good_hours,
      w$peak_score,
      w$peak_category,
      format(w$peak_time, "%H:%M", tz = tz),
      w$wave_height,
      w$wave_period,
      w$wind_speed,
      w$wind_category,
      w$dominant_limit,
      w$before_start,
      w$after_end,
      100 * w$ewam_share,
      w$status
    ))
  }
  invisible(windows)
}

# ---- Manual run --------------------------------------------------------------
# Terminal (repo root):  Rscript R/detect_windows.R
# Positron console:      source("R/detect_windows.R"); res <- run_detect()
#   then inspect res$blocks / res$windows. Working directory must be the
#   repo root (relative config/ and R/ paths).
# (Sourcing alone only defines functions: the sys.nframe() guard below is
# TRUE only under Rscript.)
run_detect <- function(now = Sys.time()) {
  source("R/fetch_forecast.R")
  source("R/score_block.R")
  source("R/daylight.R")

  spots <- load_spots()
  tiers_cfg <- load_tiers()
  win_cfg <- load_window_cfg()
  # A YAML read in a non-UTF-8 locale can silently truncate at the first
  # non-ASCII character (the comments contain ø/—) and return NULLs.
  stopifnot(
    "tiers.yaml did not parse fully (locale/encoding?)" = !is.null(tiers_cfg$category_cutoffs$good),
    "windows.yaml did not parse" = !is.null(win_cfg$hold_margin)
  )

  forecast <- fetch_all_spots(spots = spots)
  # split/map rather than group_modify: group_modify drops `spot` from .x,
  # so score_block() would see row$spot = NULL on every row.
  scored <- forecast |>
    split(~spot) |>
    map(~ score_spot(.x, tiers_cfg)) |>
    list_rbind()

  blocks <- assemble_blocks(forecast, scored, spots) |> add_daylight(win_cfg)
  windows <- detect_windows(blocks, tiers_cfg$category_cutoffs$good, win_cfg, now = now)
  print_windows(windows)
  invisible(list(blocks = blocks, windows = windows))
}

if (sys.nframe() == 0) {
  run_detect()
}

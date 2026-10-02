# score_block.R
# Applies tiers.yaml to the output of fetch_forecast.R, one row (one
# spot, one hour) at a time. Designed to be called via purrr::pmap or
# similar over the full data frame.
#
# Install once: install.packages(c("yaml", "dplyr"))

library(yaml)
library(dplyr)

load_tiers <- function(path = "config/tiers.yaml") read_yaml(path)

# ---- Category lookup -------------------------------------------------------
# cutoffs is a named list: poor, marginal, good. Returns one of
# "poor" / "marginal" / "good" / "epic".
.classify <- function(value, cutoffs) {
  if (is.na(value)) {
    return(NA_character_)
  }
  if (value < cutoffs$poor) {
    "poor"
  } else if (value < cutoffs$marginal) {
    "marginal"
  } else if (value < cutoffs$good) {
    "good"
  } else {
    "epic"
  }
}

# Numeric scores for each category, used to combine height + period into
# one swell_quality number. These weights (period valued higher than
# height, per the earlier discussion that period is the dominant quality
# signal) are a deliberate design choice, not a physical constant.
.CATEGORY_SCORE <- c(poor = 2, marginal = 4, good = 7, epic = 9.5)
PERIOD_WEIGHT <- 0.65
HEIGHT_WEIGHT <- 0.35

swell_quality <- function(height_m, period_sec, tier_cfg) {
  period_cat <- .classify(period_sec, tier_cfg$period_sec)
  height_cat <- .classify(height_m, tier_cfg$height_m)
  if (is.na(period_cat) || is.na(height_cat)) {
    return(NA_real_)
  }

  sq <- PERIOD_WEIGHT *
    .CATEGORY_SCORE[[period_cat]] +
    HEIGHT_WEIGHT * .CATEGORY_SCORE[[height_cat]]

  # Below the "poor" height cutoff, fade swell quality linearly to zero.
  # Without this, height could never pull the score below its "poor"
  # floor of 2, and period (65% weight) alone carried tiny long-period
  # swell to Good: 0.1 m @ 10 s scored the same as 0.6 m @ 10 s.
  if (height_m < tier_cfg$height_m$poor) {
    sq <- sq * max(height_m, 0) / tier_cfg$height_m$poor
  }
  sq
}

# ---- Swell direction exposure ------------------------------------------------
# How squarely the waves arrive at the spot. angle = difference between
# the direction waves come FROM and facing_deg. Full exposure up to
# full_deg, fading linearly to zero at zero_deg (waves travelling past or
# away from the beach). zero_deg > 90 allows some refraction/wrap.
# Caveat: at a 25 km GFS grid point, direction is the open-water value,
# so this catches "sea moving away from the beach" but cannot model wrap
# around a specific headland.
direction_exposure <- function(wave_dir, facing_deg, dir_cfg) {
  if (is.na(wave_dir) || is.na(facing_deg)) {
    return(NA_real_)
  }
  angle <- abs(((wave_dir - facing_deg + 180) %% 360) - 180)
  if (angle <= dir_cfg$full_deg) {
    return(1)
  }
  max(0, 1 - (angle - dir_cfg$full_deg) / (dir_cfg$zero_deg - dir_cfg$full_deg))
}

# ---- Wind direction classification, with arc wraparound handled -----------
# offshore_arc_min/max define the directional window FROM WHICH wind is
# offshore. Arcs that cross 0/360 (e.g. [320, 40]) are handled explicitly —
# this was flagged as unhandled in spots.yaml and is fixed here.
.in_arc <- function(wind_dir, arc_min, arc_max) {
  if (arc_min <= arc_max) {
    wind_dir >= arc_min & wind_dir <= arc_max
  } else {
    wind_dir >= arc_min | wind_dir <= arc_max
  }
}

# The onshore zone is the offshore arc rotated 180 degrees.
.opposite_arc <- function(arc_min, arc_max) {
  list(min = (arc_min + 180) %% 360, max = (arc_max + 180) %% 360)
}

wind_category <- function(
  wind_dir,
  wind_speed_ms,
  arc_min,
  arc_max,
  onshore_strong_ms
) {
  if (is.na(wind_dir) || is.na(wind_speed_ms)) {
    return(NA_character_)
  }

  # Strong offshore makes waves hard to catch and flattens windswell;
  # strong cross-shore chops it up. Both used to get the full modifier
  # regardless of speed. Same speed threshold as onshore_strong.
  strong <- wind_speed_ms >= onshore_strong_ms

  if (.in_arc(wind_dir, arc_min, arc_max)) {
    return(if (strong) "offshore_strong" else "offshore")
  }

  onshore <- .opposite_arc(arc_min, arc_max)
  if (.in_arc(wind_dir, onshore$min, onshore$max)) {
    return(
      if (wind_speed_ms >= onshore_strong_ms) {
        "onshore_strong"
      } else {
        "onshore_light"
      }
    )
  }
  if (strong) "cross_strong" else "cross"
}

# ---- Fetch gate for windsea blocks -----------------------------------------
# Checks whether wind has blown from the onshore (fetch-favorable) zone
# at sufficient speed for the preceding build_hrs hours. `history` is the
# full hourly data frame for this ONE spot, already sorted by datetime,
# so we can look backward from `idx`.
fetch_built <- function(
  history,
  idx,
  arc_min,
  arc_max,
  fetch_min_speed_ms,
  build_hrs
) {
  if (idx <= build_hrs) {
    return(FALSE)
  } # not enough history yet in this pull

  window <- history[(idx - build_hrs):(idx - 1), ]
  onshore <- .opposite_arc(arc_min, arc_max)

  in_fetch <- mapply(
    function(dir, spd) {
      !is.na(dir) &&
        !is.na(spd) &&
        .in_arc(dir, onshore$min, onshore$max) &&
        spd >= fetch_min_speed_ms
    },
    window$wind_direction_10m,
    window$wind_speed_10m
  )

  all(in_fetch)
}

# ---- Score one block --------------------------------------------------------
# row: one row from the fetch_forecast.R output (one spot, one hour), plus
# spot metadata (tier, offshore_arc_min/max) already joined on.
# history: the full sorted hourly data frame for this spot (for fetch_built).
# idx: row index of `row` within `history`.
score_block <- function(row, history, idx, tiers_cfg) {
  tier_cfg <- tiers_cfg$tiers[[row$tier]]
  if (is.null(tier_cfg)) {
    warning(sprintf(
      "Unknown tier '%s' for spot '%s' — skipping.",
      row$tier,
      row$spot
    ))
    return(NULL)
  }

  sq <- swell_quality(row$wave_height, row$wave_period, tier_cfg)

  # ---- Groundswell vs windsea ----
  # A block is groundswell if EITHER
  #  (a) the dedicated swell component is long-period AND carries a real
  #      share of the sea (>= min_swell_share of total height and >=
  #      min_swell_height_m). Using the swell component rather than the
  #      blended period lets a real swell under chop still count; the size
  #      checks stop a trace partition (0.06 m @ 8.6 s under 1.22 m of
  #      5 s chop, Molle 2026-10-05) from flipping the block; or
  #  (b) the blended period itself is long. Needed because GFS Wave on
  #      Open-Meteo often reports the swell partition as 0 and puts all
  #      energy in windsea, so (a) can never fire beyond the EWAM horizon.
  gs_cfg <- tiers_cfg$groundswell_classification
  if (is.null(gs_cfg)) stop("tiers.yaml: groundswell_classification block missing")
  good_period <- tier_cfg$period_sec$good

  swell_dominant <- !is.na(row$swell_wave_period) &&
    !is.na(row$swell_wave_height) &&
    !is.na(row$wave_height) &&
    row$swell_wave_period >= good_period &&
    row$swell_wave_height >= gs_cfg$min_swell_height_m &&
    row$swell_wave_height >= gs_cfg$min_swell_share * row$wave_height
  long_total <- !is.na(row$wave_period) && row$wave_period >= good_period

  is_groundswell <- swell_dominant || long_total
  block_type <- if (is_groundswell) "groundswell_block" else "windsea_block"

  # ---- Direction exposure ----
  # Swell direction when the swell component is what makes it groundswell,
  # otherwise the overall wave direction.
  wave_dir <- if (swell_dominant) row$swell_wave_direction else row$wave_direction
  if (is.null(row$facing_deg)) stop("score_block: facing_deg missing for ", row$spot)
  dir_cfg <- tiers_cfg$direction_exposure
  if (!is.null(row$exposure_full_deg) && !is.na(row$exposure_full_deg)) {
    dir_cfg$full_deg <- row$exposure_full_deg
  }
  if (!is.null(row$exposure_zero_deg) && !is.na(row$exposure_zero_deg)) {
    dir_cfg$zero_deg <- row$exposure_zero_deg
  }
  exposure <- direction_exposure(wave_dir, row$facing_deg, dir_cfg)

  # ---- Wind ----
  wcat <- wind_category(
    row$wind_direction_10m,
    row$wind_speed_10m,
    row$offshore_arc_min,
    row$offshore_arc_max,
    tier_cfg$onshore_strong_ms
  )

  wmod <- if (is.na(wcat)) NULL else tiers_cfg$wind_modifier[[block_type]][[wcat]]
  if (is.null(wmod)) {
    wmod <- NA_real_
  }

  score <- sq * exposure * wmod

  # Fetch gate only applies to windsea blocks — groundswell arrives
  # regardless of local wind duration. It is also skipped when the wind
  # is offshore: the sea is then residual (a blow that has swung
  # offshore, the classic Kattegat clean-up session), and the wave model
  # has already accounted for how much fetch built it. Gating on local
  # onshore wind there would double-count and halve the best sessions.
  # Switchable in tiers.yaml (fetch_gate$enabled) — see note there.
  fetch_ok <- TRUE
  gate_on <- !isFALSE(tiers_cfg$fetch_gate$enabled)
  if (gate_on && !is_groundswell && !(wcat %in% c("offshore", "offshore_strong"))) {
    fetch_ok <- fetch_built(
      history,
      idx,
      row$offshore_arc_min,
      row$offshore_arc_max,
      tier_cfg$fetch_min_speed_ms,
      tier_cfg$windsea_fetch_build_hrs
    )
    if (!fetch_ok) score <- score * 0.5 # cap, don't zero — matches earlier design decision
  }

  cutoffs <- tiers_cfg$category_cutoffs
  category <- if (is.na(score)) {
    NA_character_
  } else if (score < cutoffs$marginal) {
    "Flat"
  } else if (score < cutoffs$good) {
    "Marginal"
  } else if (score < cutoffs$epic) {
    "Good"
  } else {
    "Epic"
  }

  # limiting_factor: the single biggest thing capping this score, named
  # plainly — this is the piece that was the whole point of the project.
  limiting_factor <- dplyr::case_when(
    is.na(score) ~ "missing_data",
    exposure < 0.5 ~ "swell_direction",
    !is.na(wmod) & wmod < 1 ~ paste0("wind_", wcat),
    !is_groundswell & !fetch_ok ~ "fetch_not_built",
    sq < 5 ~ "weak_swell",
    exposure < 1 ~ "swell_direction",
    TRUE ~ "none"
  )

  tibble::tibble(
    spot = row$spot,
    datetime = row$datetime,
    tier = row$tier,
    block_type = block_type,
    swell_quality = round(sq, 1),
    exposure = round(exposure, 2),
    wind_category = wcat,
    wind_modifier = wmod,
    score = round(score, 1),
    category = category,
    limiting_factor = limiting_factor
  )
}

# ---- Score a full spot's hourly series -------------------------------------
score_spot <- function(spot_history, tiers_cfg) {
  spot_history <- spot_history[order(spot_history$datetime), ]
  purrr::map_dfr(seq_len(nrow(spot_history)), function(i) {
    score_block(spot_history[i, ], spot_history, i, tiers_cfg)
  })
}

# ---- Manual test --------------------------------------------------------
# source("R/fetch_forecast.R") first, then:
#   forecast <- fetch_all_spots()
#   tiers_cfg <- load_tiers()
#   scored <- forecast |> split(~spot) |>
#     purrr::map(~ score_spot(.x, tiers_cfg)) |> purrr::list_rbind()
#   (not group_modify: it strips `spot` from .x, so row$spot is NULL)
#   View(scored)
# Check limiting_factor and category by eye against what you'd expect
# for a couple of known-good or known-flat days before trusting this.

# summarise_region.R
# One plain-English sentence per region for the alert, built by rules from
# the hourly blocks (not from an AI model): every word maps to a number in
# this run's data. Covers the days spanned by that region's alerts.
#
# Swell: region-wide daily peak (max over the region's spots, daylight
#        hours only), its period, direction and type (swell vs wind-swell),
#        described as a trajectory around the peak day.
# Wind:  compass direction + speed class per day, merged into phases.
#        Deliberately NOT onshore/offshore: that differs spot to spot
#        (Hanstholm beach vs point), and the per-spot lines already say it.

library(dplyr)

# Speed classes in m/s (upper bounds, exclusive).
.WIND_CLASSES <- c(light = 5, moderate = 10, strong = Inf)

.compass8 <- function(deg) {
  pts <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  ifelse(is.na(deg), NA_character_, pts[(round(deg / 45) %% 8) + 1])
}
.compass16 <- function(deg) {
  pts <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
           "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  ifelse(is.na(deg), NA_character_, pts[(round(deg / 22.5) %% 16) + 1])
}
.vec_mean_deg <- function(deg) {
  deg <- deg[!is.na(deg)]
  if (!length(deg)) return(NA_real_)
  r <- deg * pi / 180
  (atan2(mean(sin(r)), mean(cos(r))) * 180 / pi) %% 360
}
.wind_class <- function(ms) names(.WIND_CLASSES)[findInterval(ms, c(0, .WIND_CLASSES[-length(.WIND_CLASSES)]))]

# Daily region profile: one row per local day.
region_days <- function(blocks, region_spots, days, tz) {
  b <- blocks[blocks$spot %in% region_spots & blocks$is_daylight, ]
  b$day <- as.Date(b$datetime, tz = tz)
  b <- b[b$day %in% days & !is.na(b$wave_height), ]
  if (!nrow(b)) return(NULL)

  b |>
    group_by(day) |>
    summarise(
      # The day's best-scoring hour/spot stands for the region, so the
      # sentence describes the conditions behind the alerts. (Biggest sea
      # was used first: it picked exposed-spot chop over the clean swell
      # that actually made the window, e.g. 0.8 m chop vs 0.6 m @ 10 s.)
      i = if (all(is.na(score))) which.max(wave_height)
          else order(-score, -wave_height)[1],
      height = wave_height[i],
      period = wave_period[i],
      sea_dir = if (!is.na(block_type[i]) && block_type[i] == "groundswell_block" &&
                    !is.na(swell_wave_height[i]) && swell_wave_height[i] > 0) {
        swell_wave_direction[i]
      } else {
        wave_direction[i]
      },
      is_swell = !is.na(block_type[i]) && block_type[i] == "groundswell_block",
      wind_speed = stats::median(wind_speed_10m, na.rm = TRUE),
      wind_dir = .vec_mean_deg(wind_direction_10m),
      .groups = "drop"
    ) |>
    select(-i) |>
    arrange(day)
}

.day_lab <- function(d, long) format(d, if (long) "%a %d" else "%a")
.span_lab <- function(d1, d2, long) {
  if (d1 == d2) .day_lab(d1, long) else paste0(.day_lab(d1, long), "–", .day_lab(d2, long))
}
.type <- function(is_swell) if (is_swell) "swell" else "wind-swell"
.cap <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))

# Swell trajectory around the peak day.
swell_clause <- function(p, long) {
  if (max(p$height) < 0.4) return("Small to flat throughout")
  k <- which.max(p$height)
  pk <- p[k, ]
  pk_txt <- sprintf("%.1f m @ %.0f s %s", pk$height, pk$period, .compass16(pk$sea_dir))
  first <- p[1, ]
  last <- p[nrow(p), ]

  if (nrow(p) == 1) {
    return(sprintf("%s %s %s", .cap(.type(pk$is_swell)), pk_txt, .day_lab(pk$day, long)))
  }
  if ((max(p$height) - min(p$height)) / max(p$height) < 0.2) {
    return(sprintf("%s steady around %.1f m @ %.0f s %s %s",
                   .cap(.type(pk$is_swell)), stats::median(p$height),
                   pk$period, .compass16(pk$sea_dir), .span_lab(first$day, last$day, long)))
  }

  # Lead-in: from the first day up to the peak.
  if (k == 1) {
    txt <- sprintf("%s peaks %s %s", .cap(.type(pk$is_swell)), pk_txt, .day_lab(pk$day, long))
  } else if (first$is_swell != pk$is_swell) {
    txt <- sprintf("%s %.1f m %s, then %s builds to %s %s",
                   .cap(.type(first$is_swell)), first$height, .day_lab(first$day, long),
                   .type(pk$is_swell), pk_txt, .day_lab(pk$day, long))
  } else {
    txt <- sprintf("%s builds from %.1f m %s to %s %s",
                   .cap(.type(pk$is_swell)), first$height, .day_lab(first$day, long),
                   pk_txt, .day_lab(pk$day, long))
  }
  # Tail: after the peak.
  if (k < nrow(p)) {
    if (last$height < 0.75 * pk$height) {
      txt <- sprintf("%s, easing to %.1f m %s", txt, last$height, .day_lab(last$day, long))
    } else {
      txt <- sprintf("%s, holding %s", txt, .span_lab(p$day[k + 1], last$day, long))
    }
  }
  txt
}

# Wind as phases of (speed class, 8-point direction), at most four. If
# there are more, phases on days with windows are kept (the quiet days
# dropped), so the main event day is never hidden behind "then variable".
wind_clause <- function(p, long, event_days = p$day) {
  w <- p[!is.na(p$wind_speed) & !is.na(p$wind_dir), ]
  if (!nrow(w)) return(NULL)
  w$cls <- .wind_class(w$wind_speed)
  w$dir <- .compass8(w$wind_dir)
  key <- paste(w$cls, w$dir)
  run <- cumsum(c(TRUE, key[-1] != key[-length(key)]))
  groups <- split(seq_len(nrow(w)), run)
  if (length(groups) > 4) {
    keep <- vapply(groups, function(ix) any(w$day[ix] %in% event_days), logical(1))
    groups <- groups[keep][seq_len(min(4, sum(keep)))]
  }
  phases <- lapply(groups, function(ix) {
    sprintf("%s %s %.0f–%.0f m/s %s", w$cls[ix[1]], w$dir[ix[1]],
            floor(min(w$wind_speed[ix])), ceiling(max(w$wind_speed[ix])),
            .span_lab(w$day[ix[1]], w$day[ix[length(ix)]], long))
  })
  phases <- unlist(phases)
  phases <- sub("(\\d+)–\\1 m/s", "\\1 m/s", phases) # "7–7 m/s" -> "7 m/s"
  paste0("Wind ", paste(phases, collapse = ", "))
}

# Named character vector: region -> sentence, for regions with events.
summarise_regions <- function(blocks, spots, events, tz = "Europe/Copenhagen") {
  if (!nrow(events)) return(character())
  region_of <- setNames(spots$region, spots$spot)
  ev_region <- unname(region_of[events$spot])
  out <- character()
  for (r in unique(stats::na.omit(ev_region))) {
    ev <- events[ev_region %in% r, ]
    days <- seq(min(as.Date(ev$start, tz = tz)), max(as.Date(ev$end, tz = tz)), by = "day")
    p <- region_days(blocks, spots$spot[spots$region %in% r], days, tz)
    if (is.null(p)) next
    long <- length(days) >= 7 # weekday names repeat beyond a week: add the date
    event_days <- unique(unlist(lapply(seq_len(nrow(ev)), function(k)
      as.character(seq(as.Date(ev$start[k], tz = tz), as.Date(ev$end[k], tz = tz), by = "day")))))
    parts <- c(swell_clause(p, long), wind_clause(p, long, as.Date(event_days)))
    out[[r]] <- paste0(paste(parts, collapse = ". "), ".")
  }
  out
}

# test_summarise_region.R
#   Rscript tests/test_summarise_region.R

suppressMessages({ library(dplyr); source("R/summarise_region.R") })
tz <- "Europe/Copenhagen"
spots <- tibble::tibble(spot = c("klit", "vorup", "smyge"),
                        region = c("West Jutland", "West Jutland", "South Sweden"))

# Daylight-hour blocks for one spot over given local days, one profile per day.
# prof: list of per-day lists(h, p, swell (TRUE/FALSE), dir, ws, wd)
mk <- function(spot, start_day, prof) {
  bind_rows(lapply(seq_along(prof), function(i) {
    d <- prof[[i]]
    t <- as.POSIXct(paste(as.Date(start_day) + i - 1, sprintf("%02d:00", 8:16)), tz = tz)
    tibble::tibble(spot = spot, datetime = t, is_daylight = TRUE,
      wave_height = d$h * c(0.8, 0.9, 1, 1, 1, 1, 0.9, 0.9, 0.8), wave_period = d$p,
      wave_direction = d$dir, swell_wave_height = if (d$swell) d$h * 0.9 else 0,
      swell_wave_direction = d$dir, block_type = if (d$swell) "groundswell_block" else "windsea_block",
      wind_speed_10m = d$ws, wind_direction_10m = d$wd,
      score = (if (is.null(d$sc)) d$h * 3 else d$sc) * c(0.8, 0.9, 1, 1, 1, 1, 0.9, 0.9, 0.8))
  }))
}
day <- function(h, p, swell, dir, ws, wd, sc = NULL) list(h = h, p = p, swell = swell, dir = dir, ws = ws, wd = wd, sc = sc)

# West Jutland Thu 08 -> Sun 11: small wind-swell, then a W swell peaking Sat, strong S wind Sat
wj <- list(day(0.6, 5, FALSE, 270, 4, 90), day(1.0, 6, FALSE, 270, 4, 95),
           day(2.0, 9, TRUE, 290, 12, 180), day(1.8, 8, TRUE, 280, 7, 265))
blocks <- bind_rows(mk("klit", "2026-10-08", wj),
                    mk("vorup", "2026-10-08", lapply(wj, function(d) { d$h <- d$h * 0.9; d })))
ev <- tibble::tibble(spot = c("klit", "vorup"),
                     start = as.POSIXct(c("2026-10-08 07:00", "2026-10-11 07:00"), tz = tz),
                     end = as.POSIXct(c("2026-10-08 09:00", "2026-10-11 17:00"), tz = tz))
s <- summarise_regions(blocks, spots, ev, tz)
cat(s[["West Jutland"]], "\n")
stopifnot(names(s) == "West Jutland")
stopifnot(grepl("^Wind-swell 0.6 m Thu, then swell builds to 2.0 m @ 9 s WNW Sat, holding Sun\\.", s[[1]]))
stopifnot(grepl("Wind light E 4 m/s Thu–Fri, strong S 12 m/s Sat, moderate W 7 m/s Sun\\.$", s[[1]]))

# Peak on day one, then easing
p <- region_days(mk("klit", "2026-10-08", list(day(2.0, 10, TRUE, 300, 3, 120), day(1.6, 9, TRUE, 300, 3, 120),
                                               day(1.0, 8, TRUE, 300, 3, 120))),
                 "klit", as.Date("2026-10-08") + 0:2, tz)
cat(swell_clause(p, FALSE), "\n")
stopifnot(swell_clause(p, FALSE) == "Swell peaks 2.0 m @ 10 s WNW Thu, easing to 1.0 m Sat")

# Steady, flat, and single-day cases
p <- region_days(mk("klit", "2026-10-08", rep(list(day(1.5, 8, TRUE, 270, 6, 200)), 3)), "klit",
                 as.Date("2026-10-08") + 0:2, tz)
stopifnot(grepl("^Swell steady around 1.5 m @ 8 s W Thu–Sat$", swell_clause(p, FALSE)))
p <- region_days(mk("klit", "2026-10-08", rep(list(day(0.2, 3, FALSE, 270, 3, 200)), 2)), "klit",
                 as.Date("2026-10-08") + 0:1, tz)
stopifnot(swell_clause(p, FALSE) == "Small to flat throughout")
p <- region_days(mk("smyge", "2026-10-11", list(day(1.0, 5, FALSE, 210, 7, 200))), "smyge",
                 as.Date("2026-10-11"), tz)
stopifnot(swell_clause(p, FALSE) == "Wind-swell 1.0 m @ 5 s SSW Sun")

# Spans of a week or more get dates (weekday names would repeat)
p <- region_days(mk("klit", "2026-10-02", rep(list(day(1.0, 6, FALSE, 270, 6, 200)), 8)), "klit",
                 as.Date("2026-10-02") + 0:7, tz)
stopifnot(grepl("Fri 02–Fri 09", swell_clause(p, TRUE)))

# Night hours ignored; no blocks for a region -> no sentence, no error
blocks_n <- mutate(blocks, is_daylight = FALSE)
stopifnot(length(summarise_regions(blocks_n, spots, ev, tz)) == 0)
stopifnot(length(summarise_regions(blocks, spots, ev[0, ], tz)) == 0)

# Real case 2026-10-08: biggest sea is 0.8 m of chop at one spot, but the
# window is 0.6 m @ 10 s swell at another. The sentence must describe the swell.
thu <- bind_rows(mk("klit", "2026-10-08", list(day(0.8, 5, FALSE, 260, 5, 315, sc = 2))),
                 mk("vorup", "2026-10-08", list(day(0.6, 10, TRUE, 295, 4, 315, sc = 6.6))))
p <- region_days(thu, c("klit", "vorup"), as.Date("2026-10-08"), tz)
stopifnot(swell_clause(p, FALSE) == "Swell 0.6 m @ 10 s WNW Thu")

# More than four wind phases: quiet days dropped, event days (Sat, Sun) kept
wd <- c(315, 225, 90, 180, 270, 0)  # NW, SW, E, S, W, N: six distinct phases
p6 <- region_days(mk("klit", "2026-10-06", lapply(1:6, function(i) day(1, 6, FALSE, 270, 4, wd[i]))),
                  "klit", as.Date("2026-10-06") + 0:5, tz)
wc <- wind_clause(p6, FALSE, as.Date(c("2026-10-10", "2026-10-11")))
cat(wc, "\n")
stopifnot(grepl("Sat", wc), grepl("Sun", wc), !grepl("variable", wc))

cat("All summarise_region tests passed.\n")

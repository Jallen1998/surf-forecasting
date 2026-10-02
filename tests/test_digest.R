# test_digest.R
#   Rscript tests/test_digest.R
suppressMessages({
  library(dplyr); library(purrr)
  for (f in c("R/track_windows.R", "R/notify.R", "R/summarise_region.R", "R/digest.R")) source(f)
})
tz <- "Europe/Copenhagen"
loc <- function(s) as.POSIXct(s, tz = tz)
f <- tempfile()

# ---- When is it due? (Sun 2026-10-04) ----
stopifnot(!digest_due(loc("2026-10-04 07:17"), tz, f))   # Sunday morning run: no
stopifnot(digest_due(loc("2026-10-04 19:17"), tz, f))    # Sunday evening run: yes
mark_digest_sent(loc("2026-10-04 19:17"), tz, f)
stopifnot(!digest_due(loc("2026-10-04 19:40"), tz, f))   # manual re-run same evening: no repeat
stopifnot(!digest_due(loc("2026-10-05 07:17"), tz, f))   # Monday after a good send: no
unlink(f)
stopifnot(digest_due(loc("2026-10-05 07:17"), tz, f))    # Monday after a FAILED Sunday send: retry
stopifnot(!digest_due(loc("2026-10-06 07:17"), tz, f))   # Tuesday: too late, skip stale digest
stopifnot(!digest_due(loc("2026-10-03 19:17"), tz, f))   # Saturday: no
stopifnot(digest_due(loc("2026-10-11 19:17"), tz, f))    # next Sunday: yes

# ---- Content ----
cfg <- list(display_tz = tz, page_url = "https://example/surf/")
spots <- tibble::tibble(spot = c("klit", "smyge", "gill"), region = c("West Jutland", "South Sweden", "North Zealand"),
                        name = c("Klitmøller", "Smygehuk", "Gilleleje"))
now <- loc("2026-10-04 19:17")
hrs <- as.POSIXct("2026-10-05 05:00", tz = "UTC") + (0:(7 * 24 - 1)) * 3600
blk <- function(spot, sc) tibble::tibble(spot = spot, datetime = hrs, is_daylight = TRUE, score = sc,
  wave_height = 1.5, wave_period = 8, wave_direction = 280, swell_wave_height = 0, swell_wave_direction = 0,
  block_type = "windsea_block", wind_speed_10m = 6, wind_direction_10m = 200)
blocks <- bind_rows(blk("klit", 7), blk("smyge", 5.4), blk("gill", 1))

st <- empty_state()[0, ]
mkw <- function(id, spot, start, end, sc, stage, status) {
  r <- empty_state()[NA_integer_, ]
  r$window_id <- id; r$spot <- spot; r$start <- loc(start); r$end <- loc(end); r$peak_score <- sc
  r$peak_category <- if (sc >= 8.5) "Epic" else "Good"; r$stage <- stage; r$status <- status; r
}
state <- bind_rows(
  mkw("klit-1008", "klit", "2026-10-08 08:00", "2026-10-08 14:00", 7.9, "confirmed", "active"),
  mkw("klit-1010", "klit", "2026-10-10 08:00", "2026-10-10 12:00", 8.8, "heads_up", "active"),
  mkw("klit-1001", "klit", "2026-10-01 09:00", "2026-10-01 15:00", 7.0, "confirmed", "expired"),
  mkw("smyge-0920", "smyge", "2026-09-20 09:00", "2026-09-20 12:00", 6.5, "confirmed", "expired"),  # >7 days ago
  mkw("klit-1003", "klit", "2026-10-03 09:00", "2026-10-03 15:00", 6.2, "confirmed", "cancelled"))  # never happened

d <- format_digest(state, blocks, spots, cfg, now, consider_min = 5)
cat(d, "\n")
stopifnot(grepl("week of Mon 05 Oct", d))
stopifnot(regexpr("Sat 10 Klitmøller", d) < regexpr("Thu 08 Klitmøller", d))  # best first (8.8 before 7.9)
stopifnot(grepl("Epic 8.8 \\(heads-up\\)", d), grepl("Good 7.9\n", d))
stopifnot(grepl("<b>West Jutland</b>", d), grepl("<b>South Sweden</b>", d))   # 7 and 5.4 >= consider
stopifnot(grepl("Flat: North Zealand.", d, fixed = TRUE))
stopifnot(grepl("klit-1001", d), !grepl("smyge-0920", d), !grepl("klit-1003", d))
stopifnot(grepl('href="https://example/surf/"', d), !grepl("NA|NaN", d))

# Flat week: no windows -> names the closest thing
flat <- format_digest(state[0, ], mutate(blocks, score = pmin(score, 4.2)), spots, cfg, now)
stopifnot(grepl("<b>Flat week</b>\nNothing Good+ in sight. Closest: Klitmøller", flat, fixed = TRUE))
stopifnot(grepl("No Good+ windows happened.", flat, fixed = TRUE))
cat("All digest tests passed.\n")

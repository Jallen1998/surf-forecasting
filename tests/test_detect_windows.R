# test_detect_windows.R
# Synthetic-data checks for the window rules. Run from the repo root:
#   Rscript tests/test_detect_windows.R
# No network and no testthat needed; fails loudly on the first broken rule.

source("R/daylight.R")
source("R/detect_windows.R")

cfg <- list(light = "civil", daylight_pad_min = 30, hold_margin = 1, max_gap_hours = 1, min_duration_hours = 2)
GOOD <- 6
t0 <- as.POSIXct("2026-10-03 00:00", tz = "UTC") # Klitmøller civil light ~04:50-17:40 UTC
hrs <- function(h) t0 + h * 3600

# Build one spot's 48h series; `scores` is a named vector of UTC hour -> score
# (hours not listed get score 1). Hours >= 24 are day 2.
make_spot <- function(spot, scores, n = 48, lf = NULL) {
  s <- rep(1, n)
  s[as.integer(names(scores)) + 1] <- scores
  lim <- rep("weak_swell", n)
  if (!is.null(lf)) lim[as.integer(names(lf)) + 1] <- lf
  data.frame(
    spot = spot, tier = "A_groundswell", datetime = hrs(0:(n - 1)), score = s,
    category = ifelse(s >= 8.5, "Epic", ifelse(s >= 6, "Good", ifelse(s >= 3, "Marginal", "Flat"))),
    limiting_factor = lim, wind_category = "offshore", block_type = "groundswell_block",
    marine_model = ifelse(seq_len(n) <= 29,"dwd_ewam", "ncep_gfswave025"),
    wave_height = 1.5, wave_period = 10, swell_wave_height = 1.4, swell_wave_period = 11,
    swell_wave_direction = 290, wind_speed_10m = 4, wind_direction_10m = 120,
    lat = 57.044, lon = 8.48
  )
}
v <- function(h, x) setNames(rep(x, length(h)), h)

blocks <- rbind(
  # A: hysteresis (09 at 5.5) + 1h gap bridged (12), 2h gap NOT bridged (15-16)
  make_spot("A", c(v(6:8, 7), v(9, 5.5), v(10:11, 6.5), v(12, 3), v(13:14, 6.2), v(15:16, 3), v(17:18, 7))),
  # B: Good evening + Good next morning, high scores overnight must not join them
  make_spot("B", c(v(16:18, 7), v(19:28, 8), v(29:31, 7))),
  # C: single Good hour (too short) and a hold-level-only run (never Good)
  make_spot("C", c(v(10, 7), v(12:15, 5.5))),
  # D: shoulders at hold level are trimmed; edge reasons come from neighbours
  make_spot("D", c(v(8:9, 5.5), v(10:12, 6.5), v(13, 5.5)), lf = c("9" = "fetch_not_built", "13" = "wind_onshore_light")),
  # E: series ends mid-window -> forecast_edge
  make_spot("E", v(10:11, 7), n = 12)
)
blocks$is_daylight <- is_daylight(blocks$datetime, blocks$lat, blocks$lon, cfg$light, cfg$daylight_pad_min)

now <- hrs(10)
w <- detect_windows(blocks, GOOD, cfg, now = now)
print_windows(w, tz = "UTC")

get <- function(s) w[w$spot == s, ]
h_of <- function(x) as.numeric(difftime(x, t0, units = "hours"))

# Daylight sanity: 04 UTC is dark, 05 and 18 light, 19 dark
stopifnot(identical(is_daylight(hrs(c(4, 5, 18, 19)), 57.044, 8.48, "civil", 30), c(FALSE, TRUE, TRUE, FALSE)))

A <- get("A")
stopifnot(nrow(A) == 2)
stopifnot(h_of(A$start) == c(6, 17), h_of(A$end) == c(14, 18))
stopifnot(A$n_good_hours[1] == 7) # 06-08, 10-11, 13-14
stopifnot(A$after_end[2] == "darkness")
stopifnot(A$status == c("ongoing", "upcoming"))

B <- get("B")
stopifnot(nrow(B) == 2, h_of(B$start) == c(16, 29), h_of(B$end) == c(18, 31))
stopifnot(B$after_end[1] == "darkness", B$before_start[2] == "darkness")
stopifnot(B$ewam_share[2] == 0, B$ewam_share[1] == 1) # day-2 window sits on GFS

stopifnot(nrow(get("C")) == 0)

D <- get("D")
stopifnot(nrow(D) == 1, h_of(D$start) == 10, h_of(D$end) == 12)
stopifnot(D$before_start == "fetch_not_built", D$after_end == "wind_onshore_light")

E <- get("E")
stopifnot(nrow(E) == 1, E$after_end == "forecast_edge")

# assemble_blocks must refuse duplicate hours (the DST failure mode)
fc <- data.frame(spot = "A", datetime = hrs(c(0, 1, 1)), marine_model = "dwd_ewam")
err <- tryCatch(assemble_blocks(fc, data.frame(spot = "A", datetime = hrs(0:1)), data.frame(spot = "A", lat = 0, lon = 0)), error = function(e) conditionMessage(e))
stopifnot(grepl("Duplicate", err))

# Empty case
stopifnot(nrow(detect_windows(transform(blocks, score = 1), GOOD, cfg, now)) == 0)

cat("\nAll detect_windows tests passed.\n")

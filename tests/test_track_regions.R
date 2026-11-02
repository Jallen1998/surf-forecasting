# test_track_regions.R
# Region x day alert logic: what gets pushed, and what stays silent.
#   Rscript tests/test_track_regions.R

suppressMessages({
  library(dplyr)
  source("R/track_windows.R")
  source("R/track_regions.R")
})

cfg <- list(change_start_hours = 3)
TZ <- "Europe/Copenhagen"
spots <- tibble::tibble(
  spot = c("klitmoller", "vorupoer", "hanstholm_point", "molle_havn"),
  region = c("West Jutland", "West Jutland", "West Jutland", "Mølle peninsula"),
  name = c("Klitmøller", "Vorupør", "Hanstholm point", "Mølle havn")
)
NOW <- as.POSIXct("2026-11-02 06:00", tz = "UTC")
THU <- as.POSIXct("2026-11-05 00:00", tz = "UTC")
h <- function(x, day = THU) day + x * 3600

# A state row (only the columns rollup_region_days() needs).
w <- function(spot, s, e, stage = "heads_up", cat = "Good", score = 7, status = "active", day = THU) {
  tibble::tibble(window_id = paste(spot, s), spot = spot, status = status, stage = stage,
                 start = h(s, day), end = h(e, day), peak_score = score, peak_category = cat)
}
told <- empty_told()
run <- function(state, now = NOW, deliver = TRUE) {
  r <- track_regions(state, told, spots, cfg, now = now, tz = TZ)
  if (deliver) told <<- r$told
  r$events
}
types <- function(ev) if (is.null(ev)) "" else paste(sort(ev$event_type), collapse = ",")

# Two West Jutland spots on the same day -> ONE region-day event, best spot first
ev <- run(bind_rows(w("klitmoller", 9, 14), w("vorupoer", 10, 13, score = 7.8)))
stopifnot(types(ev) == "NEW", ev$region == "West Jutland", ev$stage == "heads_up",
          ev$best_spot == "vorupoer", nrow(ev$windows[[1]]) == 2, ev$lead_days == 3)

# Same again, plus a third spot joining and a 1h wobble -> silent
ev <- run(bind_rows(w("klitmoller", 10, 14), w("vorupoer", 10, 13, score = 7.8), w("hanstholm_point", 8, 11)))
stopifnot(is.null(ev))

# Heads-up drops out entirely -> silent, and the told row is kept...
ev <- run(w("klitmoller", 1, 0)[0, ], now = NOW + 12 * 3600)
stopifnot(is.null(ev), nrow(told) == 1, told$status == "live")
# ...so when GFS brings it back unchanged, still silent (no flicker)
ev <- run(w("klitmoller", 9, 14), now = NOW + 24 * 3600)
stopifnot(is.null(ev))

# One spot gets EWAM coverage -> region-day CONFIRMED (once)
ev <- run(bind_rows(w("klitmoller", 9, 14, stage = "confirmed"), w("vorupoer", 10, 13)), now = NOW + 36 * 3600)
stopifnot(types(ev) == "CONFIRMED", ev$prev_stage == "heads_up")
ev <- run(bind_rows(w("klitmoller", 9, 14, stage = "confirmed"), w("vorupoer", 10, 13)), now = NOW + 48 * 3600)
stopifnot(is.null(ev))

# Best category goes up -> UPGRADED; region span start moves 4h -> CHANGED
ev <- run(w("klitmoller", 9, 14, stage = "confirmed", cat = "Epic", score = 9), now = NOW + 50 * 3600)
stopifnot(types(ev) == "UPGRADED", ev$prev_category == "Good")
ev <- run(w("klitmoller", 13, 16, stage = "confirmed", cat = "Epic", score = 9), now = NOW + 52 * 3600)
stopifnot(types(ev) == "CHANGED")

# A faded window still counts as live -> silent
ev <- run(w("klitmoller", 13, 16, stage = "confirmed", cat = "Epic", score = 9, status = "faded"), now = NOW + 54 * 3600)
stopifnot(is.null(ev))

# Confirmed region-day loses everything -> CANCELLED (once), then BACK if it returns
ev <- run(w("klitmoller", 1, 0)[0, ], now = NOW + 56 * 3600)
stopifnot(types(ev) == "CANCELLED", told$status == "cancelled")
ev <- run(w("klitmoller", 1, 0)[0, ], now = NOW + 58 * 3600)
stopifnot(is.null(ev))
ev <- run(w("klitmoller", 13, 16, stage = "confirmed", cat = "Epic", score = 9), now = NOW + 60 * 3600)
stopifnot(types(ev) == "BACK")

# Heads-up grade flips (GFS Good <-> Epic) are silent
told <- empty_told()
invisible(run(w("klitmoller", 9, 14)))
ev <- run(w("klitmoller", 9, 14, cat = "Epic", score = 9), now = NOW + 12 * 3600)
stopifnot(is.null(ev))
ev <- run(w("klitmoller", 9, 14), now = NOW + 24 * 3600)
stopifnot(is.null(ev))

# A confirmed region-day whose windows have simply finished is NOT cancelled
told <- empty_told()
invisible(run(w("klitmoller", 7, 10, stage = "confirmed"), now = h(-12)))
ev <- run(w("klitmoller", 7, 10, stage = "confirmed", status = "expired"), now = h(18))
stopifnot(is.null(ev))

# Different regions and different days are separate region-days
told <- empty_told()
FRI <- THU + 86400
ev <- run(bind_rows(w("klitmoller", 9, 14), w("klitmoller", 9, 14, day = FRI), w("molle_havn", 9, 12)))
stopifnot(nrow(ev) == 3)

# Failed delivery: told not updated, so the same events fire next run
told <- empty_told()
ev1 <- run(w("molle_havn", 9, 12), deliver = FALSE)
ev2 <- run(w("molle_havn", 9, 12))
stopifnot(types(ev1) == "NEW", types(ev2) == "NEW")

# Finished days drop out of the told file
ev <- run(w("klitmoller", 1, 0)[0, ], now = THU + 2 * 86400)
stopifnot(nrow(told) == 0)

# Local day, not UTC day: 23:30 UTC Wed is 00:30 Thu in Copenhagen
told <- empty_told()
late <- w("klitmoller", 0, 1); late$start <- THU - 1800; late$end <- THU + 1800
ev <- run(late)
stopifnot(ev$day == as.Date("2026-11-05"))

# Seeding on first run: built from what the spot state says was told
st <- empty_state()[1:2, ]
st$window_id <- c("a", "b"); st$spot <- c("klitmoller", "vorupoer"); st$status <- "active"
st$stage <- c("confirmed", "heads_up"); st$peak_category <- "Epic"; st$peak_score <- 9
st$start <- h(9); st$end <- h(14)
st$notified_at <- NOW; st$notified_stage <- c("heads_up", "heads_up")
st$notified_peak_category <- "Good"; st$notified_start <- h(9); st$notified_end <- h(14)
seed <- seed_told(st, spots, TZ)
stopifnot(nrow(seed) == 1, seed$stage == "heads_up", seed$category == "Good")
told <- seed
ev <- run(st)
stopifnot(types(ev) == "CONFIRMED") # the real change since the last push, nothing else

# Told file round-trips through CSV
f <- tempfile(fileext = ".csv")
write_told(told, f)
back <- read_told(f)
stopifnot(identical(back$day, told$day), identical(back$span_start, told$span_start),
          identical(back$told_at, told$told_at), back$region == told$region)
stopifnot(is.null(read_told(tempfile())))

cat("All track_regions tests passed.\n")

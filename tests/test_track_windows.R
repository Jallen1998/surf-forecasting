# test_track_windows.R
# Simulates successive runs and checks exactly which alerts fire.
#   Rscript tests/test_track_windows.R

suppressMessages({
  library(dplyr)
  source("R/track_windows.R")
})

cfg <- list(match_slack_hours = 3, confirm_ewam_share = 0.5, change_start_hours = 3,
            change_duration_frac = 0.5, change_duration_min_hours = 2, keep_past_days = 14)
HOLD <- 5
T0 <- as.POSIXct("2026-10-02 06:00", tz = "UTC")      # first run
SUN <- as.POSIXct("2026-10-11 00:00", tz = "UTC")     # the window's day
h <- function(x) SUN + x * 3600

# A detect_windows()-shaped row.
win <- function(spot, s, e, peak = 7, cat = "Good", ewam = 0, status = "upcoming") {
  tibble::tibble(spot = spot, tier = "A_groundswell", start = h(s), end = h(e),
    n_hours = e - s + 1, n_good_hours = e - s + 1, peak_time = h(s), peak_score = peak,
    peak_category = cat, mean_score = peak, wave_height = 1.9, wave_period = 8,
    swell_height = 0, swell_period = 0, swell_dir = 280, wind_speed = 7, wind_dir = 180,
    wind_category = "cross", block_type = "windsea_block", dominant_limit = "none",
    before_start = "darkness", after_end = "darkness", ewam_share = ewam, status = status, lead_hours = 0)
}
# Blocks giving the score left in a span (for faded vs cancelled).
blk <- function(spot, score) tibble::tibble(spot = spot, datetime = h(0:23), score = score, is_daylight = TRUE)
none <- blk("klitmoller", 1)

state <- empty_state()
run <- function(windows, blocks = none, now, deliver = TRUE) {
  r <- track_windows(windows, state, blocks, cfg, HOLD, now = now)
  if (deliver) state <<- mark_notified(r$state, r$events, at = now) else state <<- r$state
  r$events
}
types <- function(ev) paste(sort(ev$event_type), collapse = ",")

# Run 1: heads-up 9 days out, GFS only -> NEW
ev <- run(win("klitmoller", 5, 17), now = T0)
stopifnot(types(ev) == "NEW", ev$stage == "heads_up", ev$window_id == "klitmoller-1011")

# Run 2: GFS timing wobble of 2h -> silent (heads_up timing ignored)
ev <- run(win("klitmoller", 7, 17), now = T0 + 12 * 3600)
stopifnot(nrow(ev) == 0)

# Run 3: even a 5h GFS shift is silent while heads_up
ev <- run(win("klitmoller", 10, 17), now = T0 + 24 * 3600)
stopifnot(nrow(ev) == 0)

# Run 4: enters EWAM range -> CONFIRMED (once), same id
ev <- run(win("klitmoller", 6, 16, ewam = 0.8), now = T0 + 5 * 86400)
stopifnot(types(ev) == "CONFIRMED", ev$window_id == "klitmoller-1011", ev$prev_stage == "heads_up")

# Run 5: confirmed, 1h shift -> silent
ev <- run(win("klitmoller", 7, 16, ewam = 1), now = T0 + 5.5 * 86400)
stopifnot(nrow(ev) == 0)

# Run 6: confirmed, start moves 4h vs last alert (06:00 -> 10:00) -> CHANGED
ev <- run(win("klitmoller", 10, 16, ewam = 1), now = T0 + 6 * 86400)
stopifnot(types(ev) == "CHANGED")

# Run 7: Good -> Epic -> UPGRADED
ev <- run(win("klitmoller", 10, 16, peak = 9, cat = "Epic", ewam = 1), now = T0 + 6.5 * 86400)
stopifnot(types(ev) == "UPGRADED")

# Run 8: window vanishes but its span still scores 5.5 (hold level) -> faded, silent
ev <- run(win("klitmoller", 1, 0)[0, ], blocks = blk("klitmoller", 5.5), now = T0 + 7 * 86400)
stopifnot(nrow(ev) == 0, state$status[state$window_id == "klitmoller-1011"] == "faded")

# Run 9: comes back exactly as last told -> same id, silent
ev <- run(win("klitmoller", 10, 16, peak = 9, cat = "Epic", ewam = 1), now = T0 + 7.5 * 86400)
stopifnot(nrow(ev) == 0, state$status[state$window_id == "klitmoller-1011"] == "active")

# Run 10: vanishes and span drops to 3 -> CANCELLED
ev <- run(win("klitmoller", 1, 0)[0, ], blocks = blk("klitmoller", 3), now = T0 + 8 * 86400)
stopifnot(types(ev) == "CANCELLED", ev$window_id == "klitmoller-1011")

# Run 11: a new window at the same spot after cancellation gets a NEW id
ev <- run(win("klitmoller", 11, 15, ewam = 1), now = T0 + 8.2 * 86400)
stopifnot(types(ev) == "NEW", ev$window_id == "klitmoller-1011b")

# Failed delivery: events not marked, so they fire again next run
state <- empty_state()
ev1 <- run(win("vorupoer", 5, 12), now = T0, deliver = FALSE)
ev2 <- run(win("vorupoer", 5, 12), now = T0 + 12 * 3600)
stopifnot(types(ev1) == "NEW", types(ev2) == "NEW", ev1$window_id == ev2$window_id)

# Merge: two notified windows join into one -> one survives, other merged, no CANCELLED
state <- empty_state()
invisible(run(bind_rows(win("piren", 6, 8), win("piren", 11, 13)), now = T0))
ev <- run(win("piren", 6, 13), now = T0 + 12 * 3600)
stopifnot(!"CANCELLED" %in% ev$event_type, sum(state$status == "merged") == 1)

# Expiry: a window whose end has passed is expired silently, then pruned after keep_past_days
state <- empty_state()
invisible(run(win("smygehuk", 9, 14), now = T0))
ev <- run(win("smygehuk", 1, 0)[0, ], now = SUN + 2 * 86400)
stopifnot(nrow(ev) == 0, state$status == "expired")
invisible(run(win("smygehuk", 1, 0)[0, ], now = SUN + 20 * 86400))
stopifnot(nrow(state) == 0)

# State round-trips through CSV with timestamps intact
state <- empty_state()
invisible(run(win("klitmoller", 5, 17, ewam = 0.25), now = T0))
f <- tempfile(fileext = ".csv")
write_state(state, f)
back <- read_state(f)
stopifnot(identical(back$start, state$start), identical(back$notified_at, state$notified_at),
          back$ewam_share == 0.25, back$window_id == state$window_id)
stopifnot(identical(read_state(tempfile()), empty_state()))

cat("All track_windows tests passed.\n")

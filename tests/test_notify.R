# test_notify.R
# Message formatting + send logic, with Telegram mocked (no network).
#   Rscript tests/test_notify.R

suppressMessages({
  library(dplyr); library(purrr)
  source("R/notify.R")
})

cfg <- list(page_url = "https://example.github.io/surf/", display_tz = "Europe/Copenhagen",
            silent_if_only_heads_up = TRUE)
spots <- tibble::tibble(
  spot = c("klitmoller", "vorupoer", "molle_havn", "smygehuk"),
  region = c("West Jutland", "West Jutland", "Mølle peninsula", "South Sweden"),
  name = c("Klitmøller", "Vorupør", "Mølle havn", "Smygehuk")
)
t <- function(s) as.POSIXct(s, tz = "UTC")
ev <- function(type, spot, start, end, stage = "confirmed", cat = "Good", prev_cat = NA,
               prev_start = NA, prev_end = NA, swell_h = 0, block = "windsea_block") {
  tibble::tibble(
    event_type = type, window_id = paste0(spot, "-1011"), prev_stage = "heads_up",
    prev_start = t(prev_start), prev_end = t(prev_end), prev_peak_category = prev_cat,
    spot = spot, tier = "A_groundswell", status = "active", stage = stage,
    start = t(start), end = t(end), n_hours = 11, n_good_hours = 11, peak_time = t(start),
    peak_score = 7.0, peak_category = cat, mean_score = 6.8, wave_height = 1.9, wave_period = 8,
    swell_height = swell_h, swell_period = 0, swell_dir = 0, wave_dir = 285, wind_speed = 7,
    wind_dir = 180, wind_category = "cross", block_type = block, dominant_limit = "wind_onshore_light",
    before_start = "darkness", after_end = "darkness", ewam_share = 0.9, first_seen = t(start),
    last_seen = t(start), lead_hours = 50
  )
}
now <- t("2026-10-08 05:00:00")

events <- bind_rows(
  ev("CANCELLED", "smygehuk", "2026-10-11 07:00:00", "2026-10-11 12:00:00"),
  ev("CONFIRMED", "vorupoer", "2026-10-11 05:00:00", "2026-10-11 15:00:00"),
  ev("CHANGED", "klitmoller", "2026-10-11 09:00:00", "2026-10-11 17:00:00",
     prev_start = "2026-10-11 05:00:00", prev_end = "2026-10-11 17:00:00"),
  ev("UPGRADED", "molle_havn", "2026-10-10 06:00:00", "2026-10-10 10:00:00", cat = "Epic", prev_cat = "Good")
)
msgs <- format_alert(events, spots, cfg, now)
cat(msgs, sep = "\n=====\n"); cat("\n")

m <- msgs[1]
stopifnot(length(msgs) == 1)
stopifnot(grepl("4 updates", m))
# Regions in spots.yaml order: West Jutland, then Mølle, then South Sweden
stopifnot(regexpr("WEST JUTLAND", m) < regexpr("MØLLE PENINSULA", m),
          regexpr("MØLLE PENINSULA", m) < regexpr("SOUTH SWEDEN", m))
# Within a region, soonest first: Vorupør (05:00 UTC) before Klitmøller (09:00 UTC)
stopifnot(regexpr("Vorupør", m) < regexpr("Klitmøller", m))
# Local time (UTC+2 in October): 05:00 UTC -> 07:00
stopifnot(grepl("Sun 11 Oct 07:00", m))
# Direction: GFS swell 0 -> uses wave_dir 285 = WNW, not "N"
stopifnot(grepl("from WNW", m), !grepl("from N ", m))
stopifnot(grepl("TIMING CHANGED \\(was Sun 11 Oct 07:00–19:00\\)", m))
stopifnot(grepl("UPGRADED \\(was Good\\)", m), grepl("dropped out", m))
stopifnot(grepl('href="https://example.github.io/surf/#vorupoer-1011"', m))

# HTML escaping of anything that could break Telegram's parser
e2 <- ev("NEW", "klitmoller", "2026-10-11 05:00:00", "2026-10-11 15:00:00")
e2$dominant_limit <- "a<b&c"
stopifnot(grepl("a&lt;b&amp;c", format_alert(e2, spots, cfg, now)))

# Send logic (mocked): silent only when every alert is a heads-up
calls <- list()
mock_send <- function(text, silent) { calls[[length(calls) + 1]] <<- silent; TRUE }
hu <- ev("NEW", "klitmoller", "2026-10-11 05:00:00", "2026-10-11 15:00:00", stage = "heads_up")
stopifnot(notify_events(hu, spots, cfg, now, send = mock_send), isTRUE(calls[[1]]))
stopifnot(grepl("HEADS-UP", format_alert(hu, spots, cfg, now)),
          grepl("low confidence", format_alert(hu, spots, cfg, now)))
stopifnot(notify_events(events, spots, cfg, now, send = mock_send), isFALSE(calls[[2]]))

# A failed send reports FALSE (so run_daily does NOT mark the events as told)
stopifnot(!notify_events(events, spots, cfg, now, send = function(text, silent) FALSE))
# Nothing to send counts as delivered
stopifnot(notify_events(events[0, ], spots, cfg, now, send = function(...) stop("should not send")))

# Long alert splits under Telegram's 4096-char limit, nothing lost
big <- bind_rows(lapply(1:60, function(k) {
  x <- ev("NEW", c("klitmoller", "molle_havn", "smygehuk")[k %% 3 + 1],
          "2026-10-11 05:00:00", "2026-10-11 15:00:00")
  x$window_id <- sprintf("w%02d", k); x
}))
parts <- format_alert(big, spots, cfg, now)
stopifnot(length(parts) > 1, all(nchar(parts) <= 4096))
stopifnot(sum(sapply(sprintf("#w%02d\"", 1:60), function(id) sum(grepl(id, parts, fixed = TRUE)))) == 60)

# No credentials -> FALSE with a warning, never an error
Sys.setenv(TELEGRAM_BOT_TOKEN = "", TELEGRAM_CHAT_ID = "")
stopifnot(isFALSE(suppressWarnings(telegram_send("x"))))

cat("All notify tests passed.\n")

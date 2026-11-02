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

# Region-day events, built the way run_daily.R builds them.
suppressMessages({ source("R/track_windows.R"); source("R/track_regions.R") })
st <- function(spot, start, end, stage = "confirmed", cat = "Good", score = 7) {
  tibble::tibble(window_id = paste(spot, start), spot = spot, status = "active", stage = stage,
                 start = t(start), end = t(end), peak_score = score, peak_category = cat)
}
region_events <- function(state, told = empty_told()) {
  track_regions(state, told, spots, list(change_start_hours = 3), now = now)$events
}

# Klitmøller and Vorupør the same day -> ONE West Jutland line, spots in it
events <- region_events(bind_rows(
  st("klitmoller", "2026-10-11 08:00:00", "2026-10-11 13:00:00"),
  st("vorupoer", "2026-10-11 09:00:00", "2026-10-11 12:00:00", score = 8.1),
  st("molle_havn", "2026-10-10 06:00:00", "2026-10-10 10:00:00", cat = "Epic", score = 9)))
m <- format_alert(events, spots, cfg, now)
cat(m, "\n=====\n")
stopifnot(length(m) == 1)
stopifnot(lengths(regmatches(m, gregexpr("West Jutland", m))) == 1)
# Local times (UTC+2 in October), best spot first
stopifnot(grepl("Vorupør 11–14 · Klitmøller 10–15", m))
stopifnot(grepl("Sun 11 Oct \\(in 3 days\\)", m), grepl("GOOD 8.1", m))
# Soonest day first: Mølle (Sat) before West Jutland (Sun)
stopifnot(regexpr("Mølle peninsula", m) < regexpr("West Jutland", m))
stopifnot(grepl('href="https://example.github.io/surf/"', m))

# Each event type has its own wording
told <- track_regions(st("klitmoller", "2026-10-11 08:00:00", "2026-10-11 13:00:00", stage = "heads_up"),
                      empty_told(), spots, list(change_start_hours = 3), now = now)$told
conf <- format_alert(region_events(st("klitmoller", "2026-10-11 08:00:00", "2026-10-11 13:00:00"), told),
                     spots, cfg, now)
stopifnot(grepl("confirmed · GOOD 7.0", conf))
told$stage <- "confirmed"
up <- format_alert(region_events(st("klitmoller", "2026-10-11 08:00:00", "2026-10-11 13:00:00",
                                    cat = "Epic", score = 9.2), told), spots, cfg, now)
stopifnot(grepl("now EPIC 9.2 \\(was Good\\)", up))
off <- format_alert(region_events(st("klitmoller", "2026-10-11 08:00:00", "2026-10-11 13:00:00")[0, ], told),
                    spots, cfg, now)
stopifnot(grepl("West Jutland</b> · Sun 11 Oct", off), grepl("off", off), !grepl("Klitmøller", off))

# Heads-ups: one line per region, days listed, best spot named
hu <- region_events(bind_rows(
  st("klitmoller", "2026-10-11 08:00:00", "2026-10-11 13:00:00", stage = "heads_up"),
  st("vorupoer", "2026-10-12 08:00:00", "2026-10-12 13:00:00", stage = "heads_up", score = 7.6),
  st("molle_havn", "2026-10-12 08:00:00", "2026-10-12 13:00:00", stage = "heads_up")))
hm <- format_alert(hu, spots, cfg, now)
cat(hm, "\n=====\n")
stopifnot(grepl("Heads-ups · GFS, low confidence", hm),
          grepl("West Jutland</b> · Sun 11, Mon 12 · best GOOD 7.6 \\(Vorupør\\)", hm),
          lengths(regmatches(hm, gregexpr("🔭", hm))) == 2)

# Mixed: confirmed news before heads-ups
mix <- format_alert(bind_rows(events, hu), spots, cfg, now)
stopifnot(regexpr("🌊", mix) < regexpr("Heads-ups", mix))

# Capped: at most 6 confirmed lines, the rest pointed to the page
many <- bind_rows(lapply(1:9, function(k) st("klitmoller", sprintf("2026-10-%02d 08:00:00", 9 + k),
                                             sprintf("2026-10-%02d 12:00:00", 9 + k))))
mm <- format_alert(region_events(many), spots, cfg, now)
stopifnot(lengths(regmatches(mm, gregexpr("🌊", mm))) == 6, grepl("\\+3 more on the page", mm))

# HTML escaping of anything that could break Telegram's parser
sp2 <- spots; sp2$name[1] <- "A<b&c"
stopifnot(grepl("A&lt;b&amp;c", format_alert(region_events(st("klitmoller", "2026-10-11 08:00:00",
  "2026-10-11 13:00:00")), sp2, cfg, now)))

# Send logic (mocked): silent only when every alert is a heads-up
calls <- list()
mock_send <- function(text, silent) { calls[[length(calls) + 1]] <<- silent; TRUE }
stopifnot(notify_events(hu, spots, cfg, now, send = mock_send), isTRUE(calls[[1]]))
stopifnot(notify_events(events, spots, cfg, now, send = mock_send), isFALSE(calls[[2]]))
stopifnot(notify_events(bind_rows(events, hu), spots, cfg, now, send = mock_send), isFALSE(calls[[3]]))
# A cancellation always buzzes
stopifnot(notify_events(region_events(st("klitmoller", "2026-10-11 08:00:00", "2026-10-11 13:00:00")[0, ], told),
                        spots, cfg, now, send = mock_send), isFALSE(calls[[4]]))

# A failed send reports FALSE (so run_daily does NOT mark anything as told)
stopifnot(!notify_events(events, spots, cfg, now, send = function(text, silent) FALSE))
# Nothing to send counts as delivered
stopifnot(notify_events(NULL, spots, cfg, now, send = function(...) stop("should not send")))

# No credentials -> FALSE with a warning, never an error
Sys.setenv(TELEGRAM_BOT_TOKEN = "", TELEGRAM_CHAT_ID = "")
stopifnot(isFALSE(suppressWarnings(telegram_send("x"))))

cat("All notify tests passed.\n")

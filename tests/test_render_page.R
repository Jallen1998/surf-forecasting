# test_render_page.R: the phone page renders, links resolve, nothing leaks.
#   Rscript tests/test_render_page.R
suppressMessages({
  library(dplyr); library(purrr)
  for (f in c("R/daylight.R", "R/detect_windows.R", "R/track_windows.R", "R/notify.R",
              "R/summarise_region.R", "R/render_page.R")) source(f)
})
tz <- "Europe/Copenhagen"
now <- as.POSIXct("2026-10-03 06:00", tz = "UTC")
spots <- tibble::tibble(spot = c("klit", "smyge"), region = c("West Jutland", "South Sweden"),
                        name = c("Klitmøller", "Smygehuk"), lat = c(57.04, 55.34), lon = c(8.48, 13.36))
hrs <- as.POSIXct("2026-10-03 00:00", tz = "UTC") + (0:71) * 3600
mkb <- function(spot, lat, lon, score) tibble::tibble(spot = spot, datetime = hrs, tier = "A_groundswell",
  score = score, category = ifelse(score >= 6, "Good", "Marginal"), limiting_factor = "none",
  wind_category = "cross", block_type = "windsea_block", marine_model = ifelse(seq_along(hrs) <= 40, "dwd_ewam", "ncep_gfswave025"),
  wave_height = 1.8, wave_period = 8, wave_direction = 280, swell_wave_height = 0, swell_wave_period = 0,
  swell_wave_direction = 0, wind_speed_10m = 6, wind_direction_10m = 200, lat = lat, lon = lon)
blocks <- bind_rows(mkb("klit", 57.04, 8.48, rep(7, 72)),
                    mkb("smyge", 55.34, 13.36, rep(5.5, 72)))
blocks$is_daylight <- is_daylight(blocks$datetime, blocks$lat, blocks$lon, "civil", 30)
win_cfg <- list(light = "civil", daylight_pad_min = 30, hold_margin = 1, max_gap_hours = 1, min_duration_hours = 2)
tiers_cfg <- list(category_cutoffs = list(flat = 0, marginal = 3, good = 6, epic = 8.5))
w <- detect_windows(blocks, 6, win_cfg, now = now)
tr <- track_windows(w, empty_state(), blocks, list(match_slack_hours = 3, confirm_ewam_share = 0.5,
  change_start_hours = 3, change_duration_frac = 0.5, change_duration_min_hours = 2, keep_past_days = 14), 5, now)
state <- mark_notified(tr$state, tr$events, at = now)

f <- tempfile(fileext = ".html")
render_page(blocks, state, spots, tiers_cfg, win_cfg, list(display_tz = tz), 5, now = now, path = f)
html <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")

# Every window the alerts can link to has an anchor
stopifnot(all(sprintf("id='%s'", state$window_id[state$status == "active"]) %in%
              regmatches(html, gregexpr("id='[^']+'", html))[[1]]))
stopifnot(grepl("id='spot-klit'", html), grepl("Klitmøller", html))
# Smygehuk at 5.5 all day: in "Worth a look", not a window card
stopifnot(grepl("<li><b>Smygehuk</b>", html), !grepl("id='smyge-", html))
# Freshness stamp is the run time in UTC; stale threshold present
stopifnot(grepl("data-generated='2026-10-03T06:00:00Z'", html), grepl("data-stale-hours='14'", html))
# No leaked NA / NaN / template slots, no secrets
stopifnot(!grepl(">NA<|NaN|%s|%d", html), !grepl("TELEGRAM|api.telegram", html))
# Balanced main structural tags
for (tag in c("table", "details", "article", "main")) {
  stopifnot(lengths(regmatches(html, gregexpr(paste0("<", tag, "[ >]"), html))) ==
            lengths(regmatches(html, gregexpr(paste0("</", tag, ">"), html))))
}
cat("All render_page tests passed.\n")

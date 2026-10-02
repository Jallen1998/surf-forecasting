# digest.R
# Weekly Telegram digest: sent on the Sunday-evening run, every week, even
# when everything is flat. Separate from the event alerts (notify.R).
#
#   Best bets   top windows in the coming 7 days (or the nearest thing on a flat week)
#   Outlook     one sentence per region with anything >= "worth a look";
#               flat regions on a single line
#   Last 7 days windows that actually happened, with ids, + logging prompt
#
# state/digest_last.txt holds the local date it was last sent, so a re-run
# or a manual workflow run never sends it twice.
#
# Requires (sourced first): notify.R (.esc, .compass), summarise_region.R.

DIGEST_LAST_PATH <- "state/digest_last.txt"
DIGEST_WDAY <- 0          # POSIXlt wday: 0 = Sunday (locale-independent)
DIGEST_FROM_HOUR <- 15    # local: the evening run sends it
DIGEST_RETRY_HOURS <- 24  # a failed send is retried by the next runs for this long

# The most recent digest slot (Sunday 15:00 local) at or before `now`.
.digest_slot <- function(now, tz) {
  lt <- as.POSIXlt(now, tz = tz)
  back <- (lt$wday - DIGEST_WDAY) %% 7
  slot <- as.POSIXct(paste(as.Date(now, tz = tz) - back, sprintf("%02d:00", DIGEST_FROM_HOUR)), tz = tz)
  if (slot > now) slot <- slot - 7 * 86400
  slot
}

# Due if we're within DIGEST_RETRY_HOURS of the latest slot and it hasn't
# been sent for that slot yet. So a failed Sunday send goes out Monday
# morning instead of being lost; a long outage doesn't send a stale one.
digest_due <- function(now, tz, path = DIGEST_LAST_PATH) {
  slot <- .digest_slot(now, tz)
  last <- if (file.exists(path)) as.Date(trimws(readLines(path, warn = FALSE)[1])) else as.Date(NA)
  in_window <- as.numeric(difftime(now, slot, units = "hours")) < DIGEST_RETRY_HOURS
  in_window && (is.na(last) || last < as.Date(slot, tz = tz))
}

mark_digest_sent <- function(now, tz, path = DIGEST_LAST_PATH) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(format(as.Date(now, tz = tz)), path)
}

format_digest <- function(state, blocks, spots, cfg, now = Sys.time(), consider_min = 5) {
  tz <- cfg$display_tz
  names <- as.list(setNames(spots$name, spots$spot))
  nm <- function(s) vapply(s, function(x) names[[x]] %||% x, "")
  today <- as.Date(now, tz = tz)
  wk <- today + 1:7 # the coming Mon-Sun
  week_end <- as.numeric(as.POSIXct(format(max(wk) + 1), tz = tz)) # numeric: avoids tz-mismatch warnings

  out <- sprintf("<b>SURF WEEKLY</b> · week of %s", format(min(wk), "%a %d %b"))

  # ---- Best bets ----
  up <- state[state$status %in% c("active", "faded") & state$end >= now &
                as.numeric(state$start) < week_end, ]
  up <- up[order(-up$peak_score, up$start), ]
  if (nrow(up)) {
    top <- head(up, 3)
    lines <- sprintf("· %s %s %s–%s %s %.1f%s",
      format(top$start, "%a %d", tz = tz), nm(top$spot),
      format(top$start, "%H", tz = tz), format(top$end, "%H", tz = tz),
      top$peak_category, top$peak_score,
      ifelse(top$stage == "confirmed", "", " (heads-up)"))
    more <- if (nrow(up) > 3) sprintf("\n<i>+%d more on the page</i>", nrow(up) - 3) else ""
    out <- c(out, paste0("<b>Best bets</b>\n", .esc(paste(lines, collapse = "\n")), more))
  } else {
    b <- blocks[blocks$is_daylight & !is.na(blocks$score) &
                  as.Date(blocks$datetime, tz = tz) %in% wk, ]
    if (nrow(b)) {
      x <- b[which.max(b$score), ]
      out <- c(out, paste0("<b>Flat week</b>\n", .esc(sprintf(
        "Nothing Good+ in sight. Closest: %s %s %s, %.1f (%.1f m @ %.0f s).",
        nm(x$spot), format(x$datetime, "%a", tz = tz), format(x$datetime, "%H:%M", tz = tz),
        x$score, x$wave_height, x$wave_period))))
    } else {
      out <- c(out, "<b>Flat week</b>\nNo forecast data for the week.")
    }
  }

  # ---- Outlook ----
  pseudo <- tibble::tibble(spot = spots$spot,
    start = as.POSIXct(format(min(wk)), tz = tz) + 12 * 3600,
    end = as.POSIXct(format(max(wk)), tz = tz) + 12 * 3600)
  sentences <- summarise_regions(blocks, spots, pseudo, tz)
  bw <- blocks[blocks$is_daylight & as.Date(blocks$datetime, tz = tz) %in% wk, ]
  region_of <- setNames(spots$region, spots$spot)
  best_r <- tapply(bw$score, region_of[bw$spot], max, na.rm = TRUE)
  live_r <- names(best_r)[best_r >= consider_min]
  flat_r <- setdiff(unique(spots$region), live_r)
  ol <- vapply(intersect(unique(spots$region), live_r), function(r)
    sprintf("<b>%s</b>: <i>%s</i>", .esc(r), .esc(sentences[[r]] %||% "")), "")
  if (length(flat_r)) ol <- c(ol, .esc(sprintf("Flat: %s.", paste(flat_r, collapse = ", "))))
  out <- c(out, paste0("<b>Outlook</b>\n", paste(ol, collapse = "\n")))

  # ---- Last 7 days ----
  past <- state[state$end < now & state$end >= now - 7 * 86400 &
                  state$status %in% c("expired", "active", "faded"), ]
  past <- past[order(past$start), ]
  recap <- if (nrow(past)) {
    paste0(.esc(paste(sprintf("· %s %s %s %.1f · %s",
      format(past$start, "%a %d", tz = tz), nm(past$spot), past$peak_category,
      past$peak_score, past$window_id), collapse = "\n")),
      "\nSurfed any of these? Log it in log/recalibration_log.csv with the window id: it's how the scoring gets tuned.")
  } else {
    "No Good+ windows happened."
  }
  out <- c(out, paste0("<b>Last 7 days</b>\n", recap))

  if (!is.null(cfg$page_url) && nzchar(cfg$page_url)) {
    out <- c(out, sprintf('<a href="%s">Open the page</a>', cfg$page_url))
  }
  paste(out, collapse = "\n\n")
}

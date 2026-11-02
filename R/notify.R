# notify.R
# Formats track_regions() region-day events into one short Telegram message
# per run and sends it: one line per region and day ("West Jutland, Thu:
# Good") with the spot windows underneath; GFS heads-ups compressed to one
# line per region. Spot-level detail is on the page, not in the push.
# The bot token and chat id are read from the environment
# (TELEGRAM_BOT_TOKEN, TELEGRAM_CHAT_ID) and are never printed or logged.
#
# Install once: install.packages(c("httr", "jsonlite", "yaml"))

library(httr)
library(jsonlite)

load_notify_cfg <- function(path = "config/notify.yaml") yaml::read_yaml(path)

TELEGRAM_MAX_CHARS <- 4096

# ---- Small formatting helpers -------------------------------------------------
.esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

.compass <- function(deg) {
  if (is.na(deg)) return("?")
  pts <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
           "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  pts[(round(deg / 22.5) %% 16) + 1]
}

.pretty_limit <- function(x) {
  if (is.na(x)) return("?")
  switch(x,
    none = "nothing major",
    darkness = "darkness",
    forecast_edge = "end of forecast",
    score_dip = "conditions dip",
    weak_swell = "weak swell",
    swell_direction = "swell direction",
    fetch_not_built = "fetch not built",
    missing_data = "missing data",
    gsub("_", " ", sub("^wind_", "wind ", x))
  )
}

# Swell direction only when a real swell component drives the block; GFS
# often reports swell as 0 (which would read as "from N"), so otherwise
# use the overall wave direction.
.sea_dir <- function(e) {
  if (!is.na(e$swell_height) && e$swell_height > 0 && e$block_type == "groundswell_block") {
    e$swell_dir
  } else {
    e$wave_dir
  }
}

.when <- function(start, end, tz) {
  sprintf("%s %s–%s",
          format(start, "%a %d %b", tz = tz),
          format(start, "%H:%M", tz = tz),
          format(end, "%H:%M", tz = tz))
}

.lead_days <- function(n) {
  if (is.na(n)) return("")
  if (n <= 0) return("today")
  if (n == 1) return("tomorrow")
  sprintf("in %d days", n)
}

.cat_score <- function(category, score) {
  if (is.na(score)) toupper(category) else sprintf("%s %.1f", toupper(category), score)
}

# "Klitmøller 10–15 · Vorupør 11–14 · +1": best spot first, a spot with two
# windows that day shows both ("09–11/14–16").
.spot_detail <- function(windows, spot_names, tz, max_spots = 4) {
  if (is.null(windows) || nrow(windows) == 0) return("")
  windows <- windows[order(-windows$peak_score, windows$start), ]
  spots <- unique(windows$spot)
  parts <- vapply(spots, function(s) {
    w <- windows[windows$spot == s, ]
    w <- w[order(w$start), ]
    spans <- paste(sprintf("%s–%s", format(w$start, "%H", tz = tz), format(w$end, "%H", tz = tz)),
                   collapse = "/")
    sprintf("%s %s", spot_names[[s]] %||% s, spans)
  }, "")
  if (length(parts) > max_spots) {
    parts <- c(parts[seq_len(max_spots)], sprintf("+%d", length(parts) - max_spots))
  }
  paste(parts, collapse = " · ")
}

# One region-day event (not a heads-up) -> one or two lines.
format_region_event <- function(e, spot_names, tz) {
  day <- sprintf("%s (%s)", format(e$day, "%a %d %b"), .lead_days(e$lead_days))
  cs <- .cat_score(e$category, e$best_score)
  parts <- switch(e$event_type,
    NEW = c("🌊", cs),
    CONFIRMED = c("✅", paste("confirmed ·", cs)),
    UPGRADED = c("⬆️", sprintf("now %s (was %s)", cs, e$prev_category)),
    DOWNGRADED = c("⬇️", sprintf("now %s (was %s)", cs, e$prev_category)),
    CHANGED = c("🕐", sprintf("%s · now from %s (was %s)", cs,
                             format(e$span_start, "%H:%M", tz = tz),
                             format(e$prev_span_start, "%H:%M", tz = tz))),
    BACK = c("🔁", paste("back on ·", cs)),
    CANCELLED = c("❌", "off, dropped out of the forecast"),
    c("•", cs)
  )
  head <- sprintf("%s <b>%s</b> · %s · %s", parts[1], .esc(e$region), .esc(day), .esc(parts[2]))
  if (e$event_type == "CANCELLED") return(head)
  detail <- .spot_detail(e$windows[[1]], spot_names, tz)
  if (nzchar(detail)) paste0(head, "\n   ", .esc(detail)) else head
}

# All heads-up events for one region -> one line:
# "🔭 West Jutland · Sat 7, Sun 8 · best GOOD 7.2 (Klitmøller)"
format_region_heads_up <- function(ev, spot_names) {
  ev <- ev[order(ev$day), ]
  days <- format(ev$day, "%a %d")
  b <- which.max(ev$best_score)
  best <- sprintf("best %s (%s)", .cat_score(ev$category[b], ev$best_score[b]),
                  spot_names[[ev$best_spot[b]]] %||% ev$best_spot[b])
  sprintf("🔭 <b>%s</b> · %s · %s", .esc(ev$region[1]), .esc(paste(days, collapse = ", ")), .esc(best))
}

# Region-day events (track_regions()) -> one short Telegram message.
# Confirmed news first (soonest day first), at most `max_items` of them;
# then GFS heads-ups, one line per region. Details are on the page.
format_alert <- function(events, spots, cfg, now = Sys.time(), max_items = 6) {
  if (is.null(events) || nrow(events) == 0) return(character())
  tz <- cfg$display_tz
  spot_names <- as.list(setNames(spots$name, spots$spot))
  region_order <- c(unique(spots$region), "Other")

  is_hu <- events$stage == "heads_up" & events$event_type != "CANCELLED"
  main <- events[!is_hu, ]
  main <- main[order(main$day, -main$best_score, match(main$region, region_order)), ]
  hu <- events[is_hu, ]

  lines <- c(sprintf("<b>SURF</b> · %s", format(now, "%a %H:%M", tz = tz)))
  if (nrow(main)) {
    shown <- main[seq_len(min(nrow(main), max_items)), ]
    lines <- c(lines, "", vapply(seq_len(nrow(shown)), function(k)
      format_region_event(shown[k, ], spot_names, tz), ""))
    if (nrow(main) > max_items) {
      lines <- c(lines, sprintf("<i>+%d more on the page</i>", nrow(main) - max_items))
    }
  }
  if (nrow(hu)) {
    regions <- unique(hu$region[order(match(hu$region, region_order))])
    lines <- c(lines, "", "<i>Heads-ups · GFS, low confidence</i>",
               vapply(regions, function(r) format_region_heads_up(hu[hu$region == r, ], spot_names), ""))
  }
  if (!is.null(cfg$page_url) && nzchar(cfg$page_url)) {
    lines <- c(lines, "", sprintf('<a href="%s">Open the page</a>', cfg$page_url))
  }
  # The caps above keep this far below Telegram's limit. Backstop: drop
  # whole lines (each closes its own tags) rather than cutting HTML.
  msg <- paste(lines, collapse = "\n")
  while (nchar(msg) > TELEGRAM_MAX_CHARS - 30 && length(lines) > 2) {
    lines <- lines[-(length(lines) - 1)]
    msg <- paste(c(lines, "<i>…more on the page</i>"), collapse = "\n")
  }
  msg
}

# ---- Sending ----------------------------------------------------------------------
telegram_credentials <- function() {
  token <- Sys.getenv("TELEGRAM_BOT_TOKEN")
  chat <- Sys.getenv("TELEGRAM_CHAT_ID")
  if (!nzchar(token) || !nzchar(chat)) return(NULL)
  list(token = token, chat = chat)
}

# Returns TRUE only if Telegram confirmed delivery. Never stops the run.
telegram_send <- function(text, silent = FALSE, creds = telegram_credentials()) {
  if (is.null(creds)) {
    warning("TELEGRAM_BOT_TOKEN / TELEGRAM_CHAT_ID not set (.Renviron) — message not sent.")
    return(FALSE)
  }
  resp <- tryCatch(
    POST(
      sprintf("https://api.telegram.org/bot%s/sendMessage", creds$token),
      body = list(
        chat_id = creds$chat,
        text = text,
        parse_mode = "HTML",
        disable_web_page_preview = TRUE,
        disable_notification = silent
      ),
      encode = "json",
      timeout(20)
    ),
    error = function(e) e
  )
  if (inherits(resp, "error")) {
    # Strip the token from any error text before surfacing it.
    warning("Telegram request failed: ", gsub(creds$token, "<token>", conditionMessage(resp), fixed = TRUE))
    return(FALSE)
  }
  if (status_code(resp) != 200) {
    detail <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8"))$description,
                       error = function(e) "")
    warning(sprintf("Telegram returned HTTP %s: %s", status_code(resp), detail))
    return(FALSE)
  }
  TRUE
}

# Send the message; TRUE only if delivered (or there was nothing to send).
# `events` are region-day events from track_regions().
notify_events <- function(events, spots, cfg = load_notify_cfg(), now = Sys.time(),
                          send = telegram_send) {
  if (is.null(events) || nrow(events) == 0) return(TRUE)
  msgs <- format_alert(events, spots, cfg, now)
  silent <- isTRUE(cfg$silent_if_only_heads_up) &&
    all(events$stage == "heads_up" & events$event_type != "CANCELLED")
  ok <- vapply(msgs, function(m) isTRUE(send(m, silent = silent)), logical(1))
  all(ok)
}

notify_test <- function() {
  ok <- telegram_send(sprintf(
    "<b>Surf forecast bot</b>\nTest message from surf-forecasting at %s. Delivery works.",
    format(Sys.time(), "%a %d %b %H:%M", tz = "Europe/Copenhagen")
  ))
  if (ok) message("Test message sent — check Telegram.")
  invisible(ok)
}

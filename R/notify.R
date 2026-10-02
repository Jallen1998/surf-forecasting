# notify.R
# Formats track_windows() events into one Telegram message per run and
# sends it. The bot token and chat id are read from the environment
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

.lead <- function(hours) {
  if (is.na(hours)) return("")
  if (hours <= 0) return("now")
  if (hours < 24) return(sprintf("in %.0fh", hours))
  sprintf("in %.0f days", hours / 24)
}

.headline <- function(e, tz) {
  switch(e$event_type,
    NEW = if (e$stage == "heads_up") "HEADS-UP" else "NEW",
    CONFIRMED = "CONFIRMED",
    UPGRADED = sprintf("UPGRADED (was %s)", e$prev_peak_category),
    DOWNGRADED = sprintf("DOWNGRADED (was %s)", e$prev_peak_category),
    CHANGED = sprintf("TIMING CHANGED (was %s)", .when(e$prev_start, e$prev_end, tz)),
    CANCELLED = "CANCELLED",
    e$event_type
  )
}

# One event -> a few lines of Telegram HTML.
format_event <- function(e, spot_names, tz, page_url) {
  name <- spot_names[[e$spot]] %||% e$spot
  head <- sprintf("<b>%s</b> · %s", .esc(.headline(e, tz)), .esc(name))

  if (e$event_type == "CANCELLED") {
    body <- sprintf("%s window has dropped out of the forecast.", .when(e$start, e$end, tz))
    return(paste(head, .esc(body), sep = "\n"))
  }

  conf <- if (e$stage == "heads_up") "low confidence (GFS)" else "EWAM"
  lines <- c(
    head,
    sprintf("%s (%dh) · %s", .when(e$start, e$end, tz), as.integer(e$n_hours), .lead(e$lead_hours)),
    sprintf("Peak %s %.1f @ %s · %s", toupper(e$peak_category), e$peak_score,
            format(e$peak_time, "%H:%M", tz = tz), conf),
    sprintf("%.1f m @ %.0f s from %s · wind %.0f m/s %s",
            e$wave_height, e$wave_period, .compass(.sea_dir(e)),
            e$wind_speed, gsub("_", " ", e$wind_category)),
    sprintf("Limit: %s · ends: %s", .pretty_limit(e$dominant_limit), .pretty_limit(e$after_end))
  )
  lines[-1] <- .esc(lines[-1])
  if (!is.null(page_url) && nzchar(page_url)) {
    lines <- c(lines, sprintf('<a href="%s#%s">%s</a>', page_url, e$window_id, .esc(e$window_id)))
  }
  paste(lines, collapse = "\n")
}

# All events -> list of message strings (split if over Telegram's limit),
# grouped by region in spots.yaml order, soonest first within a region.
format_alert <- function(events, spots, cfg, now = Sys.time()) {
  if (nrow(events) == 0) return(character())
  tz <- cfg$display_tz
  region_of <- setNames(spots$region, spots$spot)
  spot_names <- as.list(setNames(spots$name, spots$spot))
  region_order <- unique(spots$region)

  events$region <- unname(region_of[events$spot])
  events$region[is.na(events$region)] <- "Other"
  events <- events[order(match(events$region, c(region_order, "Other")), events$start), ]

  header <- sprintf("<b>SURF</b> · %d update%s · run %s",
                    nrow(events), if (nrow(events) == 1) "" else "s",
                    format(now, "%a %H:%M", tz = tz))

  # Pack alert by alert under the length limit; when a message has to
  # continue, the region heading is repeated in the next one.
  msgs <- character()
  cur <- header
  cur_region <- NULL
  for (k in seq_len(nrow(events))) {
    item <- format_event(events[k, ], spot_names, tz, cfg$page_url)
    r <- events$region[k]
    piece <- function(same_region) {
      if (same_region) paste0("\n\n", item)
      else paste0("\n\n<b>", .esc(toupper(r)), "</b>\n", item)
    }
    add <- piece(identical(r, cur_region))
    if (nchar(cur) + nchar(add) > TELEGRAM_MAX_CHARS && !is.null(cur_region)) {
      msgs <- c(msgs, cur)
      cur <- paste0(header, " (cont.)")
      add <- piece(FALSE)
    }
    cur <- paste0(cur, add)
    cur_region <- r
  }
  c(msgs, cur)
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

# Send every part; delivered only if all parts went through.
notify_events <- function(events, spots, cfg = load_notify_cfg(), now = Sys.time(),
                          send = telegram_send) {
  if (nrow(events) == 0) return(TRUE)
  msgs <- format_alert(events, spots, cfg, now)
  silent <- isTRUE(cfg$silent_if_only_heads_up) &&
    all(events$event_type == "NEW" & events$stage == "heads_up")
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

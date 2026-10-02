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
    if (identical(e$dominant_limit, "none")) {
      sprintf("Ends: %s", .pretty_limit(e$after_end))
    } else {
      sprintf("Limit: %s · ends: %s", .pretty_limit(e$dominant_limit), .pretty_limit(e$after_end))
    }
  )
  lines[-1] <- .esc(lines[-1])
  if (!is.null(page_url) && nzchar(page_url)) {
    lines <- c(lines, sprintf('<a href="%s#%s">details</a>', page_url, e$window_id))
  }
  paste(lines, collapse = "\n")
}

# One heads-up -> one compact line (no link, no limit: it's 4+ days out
# and low confidence; the full card comes when it's confirmed).
format_heads_up_line <- function(e, spot_names, tz) {
  name <- spot_names[[e$spot]] %||% e$spot
  wind <- if (!is.na(e$wind_category) && grepl("_strong$", e$wind_category)) {
    sprintf(", %.0f m/s %s", e$wind_speed, sub("_strong$", "", e$wind_category))
  } else {
    ""
  }
  .esc(sprintf("· %s %s–%s %s %.1f · %.1f m %.0f s %s%s",
    name,
    format(e$start, "%H", tz = tz), format(e$end, "%H", tz = tz),
    e$peak_category, e$peak_score,
    e$wave_height, e$wave_period, .compass(.sea_dir(e)), wind))
}

# All events -> list of message strings (split under Telegram's limit).
# Full cards (confirmed / changed / cancelled ...) first, grouped by
# region; then GFS-range heads-ups as compact lines, grouped by region
# and then by day, so one swell hitting several spots reads as one event.
format_alert <- function(events, spots, cfg, now = Sys.time(), summaries = character()) {
  if (nrow(events) == 0) return(character())
  tz <- cfg$display_tz
  region_of <- setNames(spots$region, spots$spot)
  spot_names <- as.list(setNames(spots$name, spots$spot))
  region_order <- c(unique(spots$region), "Other")

  events$region <- unname(region_of[events$spot])
  events$region[is.na(events$region)] <- "Other"
  events$day <- as.Date(events$start, tz = tz)
  events <- events[order(match(events$region, region_order), events$start), ]

  is_hu <- events$event_type == "NEW" & events$stage == "heads_up"
  full <- events[!is_hu, ]
  hu <- events[is_hu, ]

  plural <- function(n, word) sprintf("%d %s%s", n, word, if (n == 1) "" else "s")
  summary <- paste(c(
    if (nrow(full)) plural(nrow(full), "update"),
    if (nrow(hu)) plural(nrow(hu), "heads-up")
  ), collapse = " + ")
  header <- sprintf("<b>SURF</b> · %s · run %s", summary, format(now, "%a %H:%M", tz = tz))

  # Pieces: (section, region, text). Section/region headings are added
  # when they change, and repeated at the top of a continuation message.
  pieces <- list()
  for (k in seq_len(nrow(full))) {
    pieces[[length(pieces) + 1]] <- list(
      section = "full", region = full$region[k],
      text = format_event(full[k, ], spot_names, tz, cfg$page_url))
  }
  if (nrow(hu)) {
    days_out <- range(pmax(hu$lead_hours, 0), na.rm = TRUE) / 24
    hu_label <- sprintf("<i>Heads-ups · low confidence: GFS, %s days out</i>",
      if (round(days_out[1]) == round(days_out[2])) sprintf("%.0f", days_out[1])
      else sprintf("%.0f–%.0f", days_out[1], days_out[2]))
    for (r in unique(hu$region)) {
      hr <- hu[hu$region == r, ]
      for (d in unique(as.character(hr$day))) {
        hd <- hr[as.character(hr$day) == d, ]
        hd <- hd[order(-hd$peak_score), ] # best spot first within a day
        lines <- vapply(seq_len(nrow(hd)), function(k) format_heads_up_line(hd[k, ], spot_names, tz), "")
        day_label <- format(hd$start[1], "%a %d", tz = tz)
        pieces[[length(pieces) + 1]] <- list(
          section = "hu", region = r,
          text = paste(c(sprintf("<u>%s</u>", day_label), lines), collapse = "\n"))
      }
    }
  }

  section_head <- function(sec) if (sec == "hu" && nrow(full) > 0) hu_label else NULL
  msgs <- character()
  cur <- if (nrow(full) == 0 && nrow(hu) > 0) paste(header, hu_label, sep = "\n") else header
  cur_sec <- if (nrow(full) == 0) "hu" else NULL
  cur_region <- NULL
  summarised <- character()
  for (p in pieces) {
    add <- ""
    if (!identical(p$section, cur_sec)) {
      h <- section_head(p$section)
      if (!is.null(h)) add <- paste0(add, "\n\n", h)
    }
    new_region <- !identical(p$section, cur_sec) || !identical(p$region, cur_region)
    if (new_region) {
      add <- paste0(add, "\n\n<b>", .esc(toupper(p$region)), "</b>")
      # Regional summary sentence, once per region per alert.
      if (!(p$region %in% summarised) && !is.na(summaries[p$region])) {
        add <- paste0(add, "\n<i>", .esc(summaries[[p$region]]), "</i>")
        summarised <- c(summarised, p$region)
      }
    }
    add <- paste0(add, if (new_region) "\n" else if (p$section == "hu") "\n" else "\n\n", p$text)

    if (nchar(cur) + nchar(add) > TELEGRAM_MAX_CHARS && !is.null(cur_region)) {
      msgs <- c(msgs, cur)
      cur <- paste0(header, " (cont.)")
      h <- section_head(p$section)
      if (p$section == "hu" && nrow(full) == 0) h <- hu_label
      add <- paste0(if (!is.null(h)) paste0("\n", h) else "",
                    "\n\n<b>", .esc(toupper(p$region)), "</b>\n", p$text)
    }
    cur <- paste0(cur, add)
    cur_sec <- p$section
    cur_region <- p$region
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
                          send = telegram_send, summaries = character()) {
  if (nrow(events) == 0) return(TRUE)
  msgs <- format_alert(events, spots, cfg, now, summaries)
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

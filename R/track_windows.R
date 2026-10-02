# track_windows.R
# Gives windows an identity across runs and decides what is worth telling
# you. Input: this run's detect_windows() output + the saved state file.
# Output: the updated state + a list of events to notify.
#
# Core rule: alerts compare each window with what you were LAST TOLD about
# it (the notified_* columns), never with the previous run. So a window
# that wobbles between runs, or fades and comes back unchanged, sends
# nothing.
#
# Window life cycle:
#   stage  heads_up  -> mostly GFS hours (low confidence)
#          confirmed -> >= confirm_ewam_share of hours from EWAM. Stage only
#                       ratchets up, so a heads_up->confirmed alert fires once.
#   status active    -> present in the latest run
#          faded     -> missing this run, but its old span still scores at
#                       hold level (just under Good). Kept silently.
#          cancelled -> missing, and its span has dropped below hold level
#          merged    -> absorbed into a neighbouring window (silent)
#          expired   -> finished (in the past)
#
# Events: NEW, CONFIRMED, UPGRADED, DOWNGRADED, CHANGED (timing), CANCELLED.
# Timing changes only count once confirmed: GFS-range timing is too noisy.
#
# State file: state/windows.csv. Datetimes stored as ISO-8601 UTC strings
# ("2026-10-11T05:00:00Z") and parsed back explicitly, never by guess.
#
# Requires: dplyr (detect_windows.R loads it).

library(dplyr)

.CATEGORY_ORDER <- c("Flat", "Marginal", "Good", "Epic")
.STAGE_ORDER <- c("heads_up", "confirmed")

.STATE_SPEC <- list(
  ts = c("start", "end", "peak_time", "first_seen", "last_seen",
         "notified_at", "notified_start", "notified_end"),
  num = c("n_hours", "n_good_hours", "peak_score", "mean_score", "wave_height",
          "wave_period", "swell_height", "swell_period", "swell_dir", "wave_dir", "wind_speed",
          "wind_dir", "ewam_share"),
  chr = c("window_id", "spot", "tier", "status", "stage", "peak_category",
          "wind_category", "block_type", "dominant_limit", "before_start",
          "after_end", "notified_stage", "notified_peak_category")
)
.STATE_COLS <- c(
  "window_id", "spot", "tier", "status", "stage", "start", "end", "n_hours",
  "n_good_hours", "peak_time", "peak_score", "peak_category", "mean_score",
  "wave_height", "wave_period", "swell_height", "swell_period", "swell_dir",
  "wave_dir", "wind_speed", "wind_dir", "wind_category", "block_type", "dominant_limit",
  "before_start", "after_end", "ewam_share", "first_seen", "last_seen",
  "notified_at", "notified_stage", "notified_start", "notified_end",
  "notified_peak_category"
)

load_tracking_cfg <- function(path = "config/windows.yaml") {
  cfg <- yaml::read_yaml(path)$tracking
  if (is.null(cfg$match_slack_hours)) stop("windows.yaml: tracking block missing")
  cfg
}

# ---- State I/O -----------------------------------------------------------------
.fmt_ts <- function(x) ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
.parse_ts <- function(x) {
  x[x == ""] <- NA
  out <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  bad <- !is.na(x) & is.na(out)
  if (any(bad)) stop("State file: unparseable timestamp(s), e.g. '", x[bad][1], "'")
  out
}

empty_state <- function() {
  cols <- lapply(.STATE_COLS, function(col) {
    if (col %in% .STATE_SPEC$ts) as.POSIXct(character(), tz = "UTC")
    else if (col %in% .STATE_SPEC$num) numeric()
    else character()
  })
  names(cols) <- .STATE_COLS
  tibble::as_tibble(cols)
}

read_state <- function(path = "state/windows.csv") {
  if (!file.exists(path)) return(empty_state())
  raw <- utils::read.csv(path, colClasses = "character", na.strings = c("", "NA"),
                         encoding = "UTF-8")
  # Columns added in later versions are filled with NA so an older state
  # file still loads; identity columns must exist.
  if (!all(c("window_id", "spot", "status", "start", "end") %in% names(raw))) {
    stop("State file is missing core columns: not a windows.csv?")
  }
  for (col in setdiff(.STATE_COLS, names(raw))) raw[[col]] <- NA_character_
  out <- tibble::as_tibble(raw[, .STATE_COLS])
  for (col in .STATE_SPEC$ts) out[[col]] <- .parse_ts(out[[col]])
  for (col in .STATE_SPEC$num) out[[col]] <- as.numeric(out[[col]])
  out
}

write_state <- function(state, path = "state/windows.csv") {
  out <- state[, .STATE_COLS]
  for (col in .STATE_SPEC$ts) out[[col]] <- .fmt_ts(out[[col]])
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(out, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(path)
}

# ---- Helpers ---------------------------------------------------------------------
.stage_of <- function(ewam_share, cfg) {
  ifelse(!is.na(ewam_share) & ewam_share >= cfg$confirm_ewam_share, "confirmed", "heads_up")
}
.max_stage <- function(a, b) {
  .STAGE_ORDER[pmax(match(a, .STAGE_ORDER), match(b, .STAGE_ORDER), na.rm = TRUE)]
}
.make_id <- function(spot, start, taken) {
  base <- paste0(spot, "-", format(start, "%m%d", tz = "Europe/Copenhagen"))
  id <- base
  k <- 1
  while (id %in% taken) {
    k <- k + 1
    id <- paste0(base, letters[k])
  }
  id
}
.hours <- function(a, b) as.numeric(difftime(a, b, units = "hours"))

# Copy this run's detected values onto a state row.
.refresh <- function(row, w) {
  for (col in intersect(names(w), .STATE_COLS)) {
    if (!col %in% c("window_id", "status", "stage")) row[[col]] <- w[[col]]
  }
  row
}

# What changed vs the last alert? Returns an event type or NA.
.diff_vs_notified <- function(row, cfg) {
  if (is.na(row$notified_at)) return("NEW")
  if (row$notified_stage == "heads_up" && row$stage == "confirmed") return("CONFIRMED")

  cat_now <- match(row$peak_category, .CATEGORY_ORDER)
  cat_was <- match(row$notified_peak_category, .CATEGORY_ORDER)
  if (!is.na(cat_now) && !is.na(cat_was) && cat_now != cat_was) {
    return(if (cat_now > cat_was) "UPGRADED" else "DOWNGRADED")
  }

  if (row$stage == "confirmed" && row$notified_stage == "confirmed") {
    start_shift <- abs(.hours(row$start, row$notified_start))
    dur_was <- .hours(row$notified_end, row$notified_start) + 1
    dur_change <- abs(row$n_hours - dur_was)
    if (start_shift >= cfg$change_start_hours ||
        (dur_change >= cfg$change_duration_min_hours &&
         dur_change >= cfg$change_duration_frac * dur_was)) {
      return("CHANGED")
    }
  }
  NA_character_
}

# Best daylight score left in [start, end] at a spot in this run's blocks.
.best_in_span <- function(blocks, spot, start, end) {
  s <- blocks$score[blocks$spot == spot & blocks$is_daylight &
                      blocks$datetime >= start & blocks$datetime <= end]
  if (length(s) == 0 || all(is.na(s))) NA_real_ else max(s, na.rm = TRUE)
}

# ---- Main ------------------------------------------------------------------------
# windows:    detect_windows() output for this run
# state:      read_state()
# blocks:     this run's blocks (for checking what's left of a vanished window)
# hold_level: good cutoff - detection hold_margin
track_windows <- function(windows, state, blocks, cfg, hold_level, now = Sys.time()) {
  slack <- cfg$match_slack_hours * 3600

  cur <- windows |> filter(status != "past")
  live_idx <- which(state$status %in% c("active", "faded"))

  # Candidate pairs: same spot, spans overlap within slack.
  pairs <- list()
  for (i in seq_len(nrow(cur))) {
    for (j in live_idx) {
      if (state$spot[j] != cur$spot[i]) next
      if (cur$start[i] <= state$end[j] + slack && cur$end[i] >= state$start[j] - slack) {
        ov <- as.numeric(min(cur$end[i], state$end[j])) - as.numeric(max(cur$start[i], state$start[j]))
        pairs[[length(pairs) + 1]] <- c(i = i, j = j, ov = ov)
      }
    }
  }
  pairs <- if (length(pairs)) as.data.frame(do.call(rbind, pairs)) else data.frame(i = integer(), j = integer(), ov = numeric())
  pairs <- pairs[order(-pairs$ov), ]

  # Greedy one-to-one assignment, largest overlap first.
  cur_to_state <- rep(NA_integer_, nrow(cur))
  state_taken <- integer()
  for (k in seq_len(nrow(pairs))) {
    i <- pairs$i[k]; j <- pairs$j[k]
    if (is.na(cur_to_state[i]) && !(j %in% state_taken)) {
      cur_to_state[i] <- j
      state_taken <- c(state_taken, j)
    }
  }

  events <- list()
  add_event <- function(type, row) {
    events[[length(events) + 1]] <<- tibble::tibble(
      event_type = type,
      window_id = row$window_id,
      prev_stage = row$notified_stage,
      prev_start = row$notified_start,
      prev_end = row$notified_end,
      prev_peak_category = row$notified_peak_category
    )
  }

  # 1. Matched windows: refresh and diff against the last alert.
  for (i in seq_len(nrow(cur))) {
    j <- cur_to_state[i]
    if (is.na(j)) next
    row <- .refresh(state[j, ], cur[i, ])
    row$status <- "active"
    row$stage <- .max_stage(row$stage, .stage_of(cur$ewam_share[i], cfg))
    row$last_seen <- now
    ev <- .diff_vs_notified(row, cfg)
    if (!is.na(ev)) add_event(ev, row)
    state[j, ] <- row
  }

  # 2. Unmatched current windows: new identities.
  new_rows <- list()
  taken_ids <- state$window_id
  for (i in which(is.na(cur_to_state))) {
    row <- .refresh(empty_state()[NA_integer_, ], cur[i, ])
    row$window_id <- .make_id(cur$spot[i], cur$start[i], taken_ids)
    taken_ids <- c(taken_ids, row$window_id)
    row$status <- "active"
    row$stage <- .stage_of(cur$ewam_share[i], cfg)
    row$first_seen <- now
    row$last_seen <- now
    add_event("NEW", row)
    new_rows[[length(new_rows) + 1]] <- row
  }

  # 3. Live state windows that matched nothing this run.
  for (j in setdiff(live_idx, state_taken)) {
    row <- state[j, ]
    absorbed <- any(pairs$j == j & !is.na(cur_to_state[pairs$i]))
    if (row$end + 3600 < now) {
      row$status <- "expired"
    } else if (absorbed) {
      row$status <- "merged"
    } else {
      best <- .best_in_span(blocks, row$spot, row$start, row$end)
      if (!is.na(best) && best >= hold_level) {
        row$status <- "faded"
      } else {
        row$status <- "cancelled"
        if (!is.na(row$notified_at)) add_event("CANCELLED", row)
      }
    }
    state[j, ] <- row
  }

  # Matched windows that have now finished.
  state$status[state$status == "active" & state$end + 3600 < now] <- "expired"

  state <- bind_rows(state, bind_rows(new_rows))

  # Prune old finished windows.
  cutoff <- now - cfg$keep_past_days * 86400
  state <- state |> filter(!(status %in% c("expired", "cancelled", "merged") & end < cutoff))

  events <- if (length(events)) bind_rows(events) else tibble::tibble(
    event_type = character(), window_id = character(), prev_stage = character(),
    prev_start = as.POSIXct(character(), tz = "UTC"), prev_end = as.POSIXct(character(), tz = "UTC"),
    prev_peak_category = character()
  )
  # Attach current window details for the message.
  events <- events |>
    left_join(state |> select(-starts_with("notified_")), by = "window_id") |>
    mutate(lead_hours = round(.hours(start, now), 1)) |>
    arrange(start, spot)

  list(state = arrange(state, start, spot), events = events)
}

# Call ONLY after the notification was actually delivered, so a failed send
# is retried next run instead of being silently marked as told.
mark_notified <- function(state, events, at = Sys.time()) {
  for (k in seq_len(nrow(events))) {
    j <- which(state$window_id == events$window_id[k])
    state$notified_at[j] <- at
    state$notified_stage[j] <- state$stage[j]
    state$notified_start[j] <- state$start[j]
    state$notified_end[j] <- state$end[j]
    state$notified_peak_category[j] <- state$peak_category[j]
  }
  state
}

print_events <- function(events, tz = "Europe/Copenhagen") {
  if (nrow(events) == 0) {
    cat("No new information: nothing to notify.\n")
    return(invisible(events))
  }
  for (k in seq_len(nrow(events))) {
    e <- events[k, ]
    cat(sprintf("%-10s %-22s %s %s-%s  %s %.1f  [%s, EWAM %.0f%%]%s\n",
      e$event_type, e$window_id,
      format(e$start, "%a %d %b", tz = tz), format(e$start, "%H:%M", tz = tz),
      format(e$end, "%H:%M", tz = tz), e$peak_category, e$peak_score,
      e$stage, 100 * e$ewam_share,
      if (!is.na(e$prev_start)) sprintf("  (was %s-%s %s)",
        format(e$prev_start, "%a %H:%M", tz = tz), format(e$prev_end, "%H:%M", tz = tz),
        e$prev_peak_category) else ""))
  }
  invisible(events)
}

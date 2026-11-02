# track_regions.R
# Decides what gets PUSHED, one level above track_windows.R.
#
# I don't decide "is Vorupør 11-14 good?", I decide "do I drive to West
# Jutland on Thursday?". So alerts are about region x local day, not about
# individual spot windows:
#   - all live windows (active or faded) in one region on one local day form
#     one region-day. Its stage is the best stage (any spot confirmed ->
#     confirmed), its category the best peak category.
#   - each region-day is compared with what I was LAST TOLD about it
#     (state/regions_told.csv), never with the previous run.
#
# Region-day events:
#   NEW        first time I hear about this region-day (heads_up or confirmed)
#   CONFIRMED  told as heads-up, now confirmed
#   UPGRADED / DOWNGRADED  confirmed, and best category changed vs what I was told
#   CHANGED    confirmed, and the region's span start moved >= change_start_hours
#   CANCELLED  told as confirmed, and nothing live is left that day (windows
#              that have simply finished don't count)
#   BACK       told CANCELLED, and it has come back
#
# Silent on purpose:
#   - a heads-up region-day changing grade (GFS noise; ~40 pushes a month).
#   - a heads-up region-day that drops out. The told row is kept, so if GFS
#     brings it back unchanged nothing is sent (this stops the on/off
#     flicker of marginal GFS days).
#   - another spot joining a region-day I already know about, or spot
#     windows shifting by an hour or two. That detail lives on the page.
#
# Told rows are kept until their day is over.

.TOLD_COLS <- c("region", "day", "stage", "category", "span_start", "status", "told_at")

empty_told <- function() {
  tibble::tibble(region = character(), day = as.Date(character()), stage = character(),
                 category = character(), span_start = as.POSIXct(character(), tz = "UTC"),
                 status = character(), told_at = as.POSIXct(character(), tz = "UTC"))
}

read_told <- function(path = "state/regions_told.csv") {
  if (!file.exists(path)) return(NULL) # NULL = never run before (see seed_told)
  raw <- utils::read.csv(path, colClasses = "character", na.strings = c("", "NA"),
                         encoding = "UTF-8")
  if (nrow(raw) == 0) return(empty_told())
  tibble::tibble(
    region = raw$region,
    day = as.Date(raw$day),
    stage = raw$stage,
    category = raw$category,
    span_start = .parse_ts(raw$span_start),
    status = raw$status,
    told_at = .parse_ts(raw$told_at)
  )
}

write_told <- function(told, path = "state/regions_told.csv") {
  out <- as.data.frame(told[, .TOLD_COLS])
  out$day <- format(out$day, "%Y-%m-%d")
  out$span_start <- .fmt_ts(out$span_start)
  out$told_at <- .fmt_ts(out$told_at)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(out, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(path)
}

# Live spot windows -> one row per region x local day, plus the spot detail
# for the message.
rollup_region_days <- function(state, spots, tz = "Europe/Copenhagen") {
  region_of <- setNames(spots$region, spots$spot)
  live <- state[state$status %in% c("active", "faded"), ]
  if (nrow(live) == 0) {
    return(tibble::tibble(region = character(), day = as.Date(character()), stage = character(),
                          category = character(), best_score = numeric(), best_spot = character(),
                          span_start = as.POSIXct(character(), tz = "UTC"),
                          span_end = as.POSIXct(character(), tz = "UTC"), windows = list()))
  }
  live$region <- unname(region_of[live$spot])
  live$region[is.na(live$region)] <- "Other"
  live$day <- as.Date(live$start, tz = tz)
  keys <- unique(live[, c("region", "day")])
  rows <- lapply(seq_len(nrow(keys)), function(k) {
    w <- live[live$region == keys$region[k] & live$day == keys$day[k], ]
    w <- w[order(-w$peak_score, w$start), ]
    tibble::tibble(
      region = keys$region[k],
      day = keys$day[k],
      stage = if (any(w$stage == "confirmed")) "confirmed" else "heads_up",
      category = .CATEGORY_ORDER[max(match(w$peak_category, .CATEGORY_ORDER), na.rm = TRUE)],
      best_score = w$peak_score[1],
      best_spot = w$spot[1],
      span_start = min(w$start),
      span_end = max(w$end),
      windows = list(w[, c("spot", "start", "end", "peak_score", "peak_category", "stage")])
    )
  })
  dplyr::bind_rows(rows)
}

# First run after this change: there is no regions_told.csv yet. Seed it
# from what the spot-level state says I was already told, so deploying this
# doesn't re-announce every live region-day in one burst.
seed_told <- function(state, spots, tz = "Europe/Copenhagen") {
  told_rows <- state[!is.na(state$notified_at) & state$status %in% c("active", "faded"), ]
  if (nrow(told_rows) == 0) return(empty_told())
  told_rows$stage <- told_rows$notified_stage
  told_rows$peak_category <- told_rows$notified_peak_category
  told_rows$start <- told_rows$notified_start
  told_rows$end <- told_rows$notified_end
  rd <- rollup_region_days(told_rows, spots, tz)
  tibble::tibble(region = rd$region, day = rd$day, stage = rd$stage, category = rd$category,
                 span_start = rd$span_start, status = "live",
                 told_at = max(told_rows$notified_at))
}

# state: spot-level state AFTER track_windows() this run.
# told:  read_told() (or seed_told() on the first run).
# Returns list(events, told). `told` is what the file should hold IF this
# run's message is delivered; on a failed send keep the old one so it
# retries next run.
track_regions <- function(state, told, spots, cfg, now = Sys.time(), tz = "Europe/Copenhagen") {
  today <- as.Date(now, tz = tz)
  told <- told[told$day >= today, ] # finished days drop out
  cur <- rollup_region_days(state, spots, tz)
  cur <- cur[cur$day >= today, ]

  key <- function(r, d) paste(r, format(d, "%Y-%m-%d"))
  cur_key <- key(cur$region, cur$day)
  told_key <- key(told$region, told$day)

  events <- list()
  add <- function(type, c_row, t_row = NULL) {
    events[[length(events) + 1]] <<- tibble::tibble(
      event_type = type,
      region = if (!is.null(c_row)) c_row$region else t_row$region,
      day = if (!is.null(c_row)) c_row$day else t_row$day,
      stage = if (!is.null(c_row)) c_row$stage else t_row$stage,
      category = if (!is.null(c_row)) c_row$category else t_row$category,
      best_score = if (!is.null(c_row)) c_row$best_score else NA_real_,
      best_spot = if (!is.null(c_row)) c_row$best_spot else NA_character_,
      span_start = if (!is.null(c_row)) c_row$span_start else t_row$span_start,
      span_end = if (!is.null(c_row)) c_row$span_end else as.POSIXct(NA, tz = "UTC"),
      windows = if (!is.null(c_row)) c_row$windows else list(NULL),
      prev_stage = if (!is.null(t_row)) t_row$stage else NA_character_,
      prev_category = if (!is.null(t_row)) t_row$category else NA_character_,
      prev_span_start = if (!is.null(t_row)) t_row$span_start else as.POSIXct(NA, tz = "UTC")
    )
  }
  upsert <- function(c_row, status) {
    new <- tibble::tibble(region = c_row$region, day = c_row$day, stage = c_row$stage,
                          category = c_row$category, span_start = c_row$span_start,
                          status = status, told_at = now)
    hit <- which(told_key == key(c_row$region, c_row$day))
    if (length(hit)) told[hit, ] <<- new else {
      told <<- dplyr::bind_rows(told, new)
      told_key <<- c(told_key, key(c_row$region, c_row$day))
    }
  }

  # 1. Region-days with live windows.
  for (k in seq_len(nrow(cur))) {
    c_row <- cur[k, ]
    hit <- which(told_key == cur_key[k])
    if (!length(hit)) {
      add("NEW", c_row); upsert(c_row, "live"); next
    }
    t_row <- told[hit, ]
    if (identical(t_row$status, "cancelled")) {
      add("BACK", c_row, t_row); upsert(c_row, "live"); next
    }
    if (t_row$stage == "heads_up" && c_row$stage == "confirmed") {
      add("CONFIRMED", c_row, t_row); upsert(c_row, "live"); next
    }
    cat_now <- match(c_row$category, .CATEGORY_ORDER)
    cat_was <- match(t_row$category, .CATEGORY_ORDER)
    # Grade changes only count once confirmed: GFS-range region-days flip
    # Good <-> Epic from run to run. The CONFIRMED message carries the
    # grade at that point anyway.
    if (c_row$stage == "confirmed" && !is.na(cat_now) && !is.na(cat_was) && cat_now != cat_was) {
      add(if (cat_now > cat_was) "UPGRADED" else "DOWNGRADED", c_row, t_row)
      upsert(c_row, "live"); next
    }
    if (c_row$stage == "confirmed" && t_row$stage == "confirmed" && !is.na(t_row$span_start) &&
        abs(.hours(c_row$span_start, t_row$span_start)) >= cfg$change_start_hours) {
      add("CHANGED", c_row, t_row); upsert(c_row, "live"); next
    }
    # Otherwise: nothing I'd act on differently. Silent.
  }

  # 2. Told region-days with nothing live left.
  # Windows that simply finished (expired, e.g. the evening run after a
  # morning session) are not a cancellation.
  region_of <- setNames(spots$region, spots$spot)
  done <- state[state$status == "expired", ]
  done_key <- key(unname(region_of[done$spot]), as.Date(done$start, tz = tz))
  for (j in which(!(told_key %in% cur_key) & told$status == "live")) {
    t_row <- told[j, ]
    if (told_key[j] %in% done_key) next
    if (t_row$stage == "confirmed") {
      add("CANCELLED", NULL, t_row)
      told$status[j] <- "cancelled"
      told$told_at[j] <- now
    }
    # A heads-up that drops out: silent, told row kept (anti-flicker).
  }

  events <- if (length(events)) dplyr::bind_rows(events) else NULL
  if (!is.null(events)) {
    events$lead_days <- as.integer(events$day - today)
    events <- events[order(events$day, -match(events$category, .CATEGORY_ORDER)), ]
  }
  list(events = events, told = told[order(told$day, told$region), ])
}

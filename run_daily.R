# run_daily.R
# One pipeline run: fetch -> score -> detect windows -> track vs state ->
# notify -> save state. Will run twice daily (~07:00 and ~19:00 local).
#
# From the repo root:
#   Rscript run_daily.R              real run: sends Telegram alert, updates state/windows.csv
#   Rscript run_daily.R --dry-run    prints the message it WOULD send; no send, state untouched
#   Rscript run_daily.R --dry-run --page   ...and writes a page preview to scratch/preview.html
#   Rscript run_daily.R --dry-run --digest ...and prints this week's digest
#
# The weekly digest goes out on the Sunday-evening real run (R/digest.R);
# add --digest to a real run to force one.
#
# A real run also rebuilds docs/index.html (the phone page). Dry runs never
# touch docs/: the bot commits that file, and a local copy would conflict.
#
# Telegram credentials come from .Renviron (TELEGRAM_BOT_TOKEN,
# TELEGRAM_CHAT_ID). Without them nothing is sent and nothing is marked as
# told, so the alerts go out on the first run that can deliver them.
#
# Run it twice in a row: the second run should print "No new information".
# That's the core promise (only alert on new information) working.

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args

source("R/detect_windows.R")
source("R/track_windows.R")
source("R/track_regions.R")
source("R/notify.R")
source("R/summarise_region.R")
source("R/render_page.R")
source("R/digest.R")

now <- Sys.time()
res <- run_detect(now = now) # sources fetch/score/daylight, prints windows

tiers_cfg <- load_tiers()
win_cfg <- load_window_cfg()
trk_cfg <- load_tracking_cfg()
hold_level <- tiers_cfg$category_cutoffs$good - win_cfg$hold_margin

state <- read_state()
tr <- track_windows(
  res$windows,
  state,
  res$blocks,
  trk_cfg,
  hold_level,
  now = now
)

cat("\n---- Spot-level changes this run ----\n")
print_events(tr$events)

spots <- load_spots()
notify_cfg <- load_notify_cfg()
consider_min <- yaml::read_yaml("config/windows.yaml")$consider$min_score %||% 5

# What gets pushed is decided per region and day (R/track_regions.R),
# against state/regions_told.csv. First run without that file: seed it from
# the spot-level "told" columns so nothing already sent is re-announced.
told <- read_told()
if (is.null(told)) told <- seed_told(state, spots, notify_cfg$display_tz)
rg <- track_regions(tr$state, told, spots, trk_cfg, now = now, tz = notify_cfg$display_tz)
cat("\n---- Region-day alerts this run ----\n")
if (is.null(rg$events)) {
  cat("Nothing to push.\n")
} else {
  for (k in seq_len(nrow(rg$events))) {
    e <- rg$events[k, ]
    cat(sprintf("%-10s %-16s %s  %s %s\n", e$event_type, e$region,
                format(e$day, "%a %d %b"), e$stage, e$category))
  }
}

if (dry_run) {
  msgs <- format_alert(rg$events, spots, notify_cfg, now)
  if (length(msgs)) {
    cat("\n---- Telegram message (dry run, not sent) ----\n")
    cat(msgs, sep = "\n\n[next message]\n\n")
    cat("\n")
  }
  if ("--page" %in% args) {
    render_page(
      res$blocks,
      tr$state,
      spots,
      tiers_cfg,
      win_cfg,
      notify_cfg,
      consider_min,
      now = now,
      path = "scratch/preview.html"
    )
    cat("\nPage preview written to scratch/preview.html\n")
  }
  if ("--digest" %in% args) {
    cat("\n---- Weekly digest (dry run, not sent) ----\n")
    cat(
      format_digest(tr$state, res$blocks, spots, notify_cfg, now, consider_min),
      "\n"
    )
  }
  cat("\nDry run: nothing sent, state/ not changed.\n")
} else {
  # mark_notified() / write_told() only on confirmed delivery, so a failed
  # send is retried next run instead of being silently marked as told.
  delivered <- notify_events(rg$events, spots, notify_cfg, now)
  if (!is.null(rg$events)) {
    cat(
      if (delivered) {
        "Telegram: delivered.\n"
      } else {
        "Telegram: NOT delivered (see warning) — will retry next run.\n"
      }
    )
  }
  new_state <- if (delivered) {
    mark_notified(tr$state, tr$events, at = now)
  } else {
    tr$state
  }
  write_state(new_state)
  if (delivered) write_told(rg$told)
  render_page(
    res$blocks,
    new_state,
    spots,
    tiers_cfg,
    win_cfg,
    notify_cfg,
    consider_min,
    now = now
  )
  cat("Page rebuilt: docs/index.html\n")

  if ("--digest" %in% args || digest_due(now, notify_cfg$display_tz)) {
    sent <- telegram_send(format_digest(
      new_state,
      res$blocks,
      spots,
      notify_cfg,
      now,
      consider_min
    ))
    if (sent) {
      mark_digest_sent(now, notify_cfg$display_tz)
    }
    cat(
      if (sent) {
        "Weekly digest: delivered.\n"
      } else {
        "Weekly digest: NOT delivered — will retry next run.\n"
      }
    )
  }
  cat(sprintf(
    "\nState saved: %d windows tracked (%s).\n",
    nrow(new_state),
    paste(
      names(table(new_state$status)),
      table(new_state$status),
      sep = "=",
      collapse = ", "
    )
  ))
}

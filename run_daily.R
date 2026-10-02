# run_daily.R
# One pipeline run: fetch -> score -> detect windows -> track vs state ->
# notify -> save state. Will run twice daily (~07:00 and ~19:00 local).
#
# From the repo root:
#   Rscript run_daily.R              real run: prints events, updates state/windows.csv
#   Rscript run_daily.R --dry-run    prints events, leaves state untouched
#
# Run it twice in a row: the second run should print "No new information".
# That's the core promise (only alert on new information) working.

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args

source("R/detect_windows.R")
source("R/track_windows.R")

now <- Sys.time()
res <- run_detect(now = now) # sources fetch/score/daylight, prints windows

tiers_cfg <- load_tiers()
win_cfg <- load_window_cfg()
trk_cfg <- load_tracking_cfg()
hold_level <- tiers_cfg$category_cutoffs$good - win_cfg$hold_margin

state <- read_state()
tr <- track_windows(res$windows, state, res$blocks, trk_cfg, hold_level, now = now)

cat("\n---- Alerts this run ----\n")
print_events(tr$events)

# Delivery is the console until R/notify.R (Telegram) exists. When it
# does, `delivered` must be the send result: mark_notified() only runs on
# a successful send, so a failed send is retried on the next run.
delivered <- TRUE

if (dry_run) {
  cat("\nDry run: state/windows.csv not changed.\n")
} else {
  new_state <- if (delivered) mark_notified(tr$state, tr$events, at = now) else tr$state
  write_state(new_state)
  cat(sprintf(
    "\nState saved: %d windows tracked (%s).\n",
    nrow(new_state),
    paste(names(table(new_state$status)), table(new_state$status), sep = "=", collapse = ", ")
  ))
}

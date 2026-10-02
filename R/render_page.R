# render_page.R
# Builds docs/index.html: the phone page the Telegram "details" links point
# to (GitHub Pages serves /docs on main). One self-contained static file,
# rebuilt every run. Nothing secret goes in it: the repo and page are public.
#
# Requires (sourced first): detect_windows.R, track_windows.R, notify.R
# (.esc, .compass, .pretty_limit, .sea_dir), summarise_region.R.

STALE_AFTER_HOURS <- 14 # one missed twice-daily run

.h <- function(x) .esc(as.character(x))
.loc <- function(t, fmt, tz) format(t, fmt, tz = tz)

# Score -> CSS class. Same cut-offs as tiers.yaml plus the "consider" band.
.score_class <- function(s, cuts, consider) {
  ifelse(is.na(s), "na",
  ifelse(s >= cuts$epic, "epic",
  ifelse(s >= cuts$good, "good",
  ifelse(s >= consider, "consider",
  ifelse(s >= cuts$marginal, "marginal", "flat")))))
}

# Best daylight hour per spot per local day (for the grid and spot details).
.daily_best <- function(blocks, tz, days) {
  b <- blocks[blocks$is_daylight & !is.na(blocks$score), ]
  b$day <- as.Date(b$datetime, tz = tz)
  b <- b[b$day %in% days, ]
  if (!nrow(b)) return(NULL)
  b <- b[order(b$spot, b$day, -b$score, -b$wave_height), ]
  b[!duplicated(b[, c("spot", "day")]), ]
}

# ---- Sections ------------------------------------------------------------------
.window_card <- function(w, names, tz, cuts, consider) {
  cls <- .score_class(w$peak_score, cuts, consider)
  stage <- if (w$status == "faded") "weakened" else if (w$stage == "confirmed") "confirmed" else "heads-up"
  sea_dir <- .compass(if (!is.na(w$swell_height) && w$swell_height > 0 &&
                          identical(w$block_type, "groundswell_block")) w$swell_dir else w$wave_dir)
  limit <- if (identical(w$dominant_limit, "none")) "" else
    sprintf("<div class='muted'>Limit: %s</div>", .h(.pretty_limit(w$dominant_limit)))
  sprintf(
"<article class='card %s' id='%s'>
  <div class='card-top'><span class='spot'>%s</span><span class='badge %s'>%s</span></div>
  <div class='when'>%s %s–%s <span class='muted'>(%dh)</span></div>
  <div class='peak'><span class='score'>%.1f</span> %s · peak %s</div>
  <div>%.1f m @ %.0f s from %s · wind %.0f m/s %s</div>
  %s
  <div class='muted small'>Ends: %s · EWAM %.0f%%</div>
</article>",
    cls, .h(w$window_id), .h(names[[w$spot]] %||% w$spot), gsub("[^a-z]", "", stage), stage,
    .loc(w$start, "%a %d %b", tz), .loc(w$start, "%H:%M", tz), .loc(w$end, "%H:%M", tz),
    as.integer(w$n_hours), w$peak_score, .h(w$peak_category), .loc(w$peak_time, "%H:%M", tz),
    w$wave_height, w$wave_period, sea_dir, w$wind_speed, .h(gsub("_", " ", w$wind_category)),
    limit, .h(.pretty_limit(w$after_end)), 100 * w$ewam_share)
}

.grid <- function(best, spots, days, tz, cuts, consider, gfs_days) {
  head <- paste0("<th></th>", paste(sprintf("<th class='%s'>%s<br><span class='small'>%s</span></th>",
    ifelse(days %in% gfs_days, "gfs", ""), format(days, "%a"), format(days, "%d")), collapse = ""))
  rows <- character()
  for (r in unique(spots$region)) {
    rows <- c(rows, sprintf("<tr class='region-row'><td colspan='%d'>%s</td></tr>", length(days) + 1, .h(r)))
    for (s in spots$spot[spots$region == r]) {
      cells <- vapply(days, function(d) {
        x <- best[best$spot == s & best$day == d, ]
        if (!nrow(x)) return("<td class='na'>·</td>")
        sprintf("<td class='%s%s' title='%s %s: %.1f %s'>%.0f</td>",
          .score_class(x$score, cuts, consider), if (d %in% gfs_days) " gfs" else "",
          .h(spots$name[spots$spot == s]), .loc(x$datetime, "%a %H:%M", tz), x$score,
          .h(.pretty_limit(x$limiting_factor)), x$score)
      }, "")
      rows <- c(rows, sprintf("<tr><th class='rowname'><a href='#spot-%s'>%s</a></th>%s</tr>",
        s, .h(spots$name[spots$spot == s]), paste(cells, collapse = "")))
    }
  }
  sprintf("<div class='grid-wrap'><table class='grid'><thead><tr>%s</tr></thead><tbody>%s</tbody></table></div>",
          head, paste(rows, collapse = "\n"))
}

.spot_details <- function(best, spots, tz, cuts, consider) {
  out <- character()
  for (r in unique(spots$region)) {
    out <- c(out, sprintf("<h3>%s</h3>", .h(r)))
    for (s in spots$spot[spots$region == r]) {
      x <- best[best$spot == s, ]
      top <- if (nrow(x)) max(x$score) else NA
      rows <- if (!nrow(x)) "<tr><td colspan='5' class='muted'>No data</td></tr>" else
        paste(vapply(seq_len(nrow(x)), function(k) {
          y <- x[k, ]
          lim <- if (identical(y$limiting_factor, "none")) "–" else .pretty_limit(y$limiting_factor)
          sprintf("<tr><td>%s<br><span class='muted'>%s</span></td><td class='%s num'>%.1f</td><td>%.1f&nbsp;m · %.0f&nbsp;s<br><span class='muted'>%s</span></td><td>%.0f&nbsp;m/s<br><span class='muted'>%s</span></td><td class='muted'>%s</td></tr>",
            .loc(y$datetime, "%a %d", tz), .loc(y$datetime, "%H:%M", tz),
            .score_class(y$score, cuts, consider), y$score,
            y$wave_height, y$wave_period, .compass(y$wave_direction),
            y$wind_speed_10m, .compass(y$wind_direction_10m), .h(lim))
        }, ""), collapse = "")
      out <- c(out, sprintf(
"<details id='spot-%s'><summary><span>%s</span><span class='%s pill'>best %s</span></summary>
<table class='detail'><thead><tr><th>Best hour</th><th>Score</th><th>Sea</th><th>Wind</th><th>Limit</th></tr></thead><tbody>%s</tbody></table></details>",
        s, .h(spots$name[spots$spot == s]), .score_class(top, cuts, consider),
        if (is.na(top)) "–" else sprintf("%.1f", top), rows))
    }
  }
  paste(out, collapse = "\n")
}

# ---- Page ------------------------------------------------------------------------
render_page <- function(blocks, state, spots, tiers_cfg, win_cfg, notify_cfg, consider_min,
                        now = Sys.time(), path = "docs/index.html", horizon_days = 10) {
  tz <- notify_cfg$display_tz
  cuts <- tiers_cfg$category_cutoffs
  names <- as.list(setNames(spots$name, spots$spot))
  today <- as.Date(now, tz = tz)
  days <- today + 0:(horizon_days - 1)

  best <- .daily_best(blocks, tz, days)
  if (is.null(best)) best <- blocks[0, ]
  # Days where most of the region's daylight hours are GFS: shown fainter.
  bd <- blocks[blocks$is_daylight, ]
  bd$day <- as.Date(bd$datetime, tz = tz)
  gfs_share <- tapply(bd$marine_model != "dwd_ewam", bd$day, mean)
  gfs_days <- as.Date(names(gfs_share)[gfs_share > 0.5])

  # Windows: active and faded, upcoming/ongoing, soonest first.
  live <- state[state$status %in% c("active", "faded") & state$end + 3600 >= now, ]
  live <- live[order(live$start), ]
  cards <- if (nrow(live)) paste(vapply(seq_len(nrow(live)), function(k)
    .window_card(live[k, ], names, tz, cuts, consider_min), ""), collapse = "\n") else
    "<p class='muted'>No Good+ windows in the next 10 days.</p>"

  # Worth a look: windows detected at the consider level that never reach Good
  # and don't overlap a Good window at the same spot.
  cw <- detect_windows(blocks, consider_min, win_cfg, now = now)
  cw <- cw[cw$status != "past" & cw$peak_score < cuts$good, ]
  if (nrow(cw) && nrow(live)) {
    overl <- vapply(seq_len(nrow(cw)), function(i) any(live$spot == cw$spot[i] &
      live$start <= cw$end[i] & live$end >= cw$start[i]), logical(1))
    cw <- cw[!overl, ]
  }
  consider_html <- if (nrow(cw)) paste0("<ul class='consider-list'>", paste(sprintf(
    "<li><b>%s</b> %s %s–%s · %.1f · %.1f m %.0f s · wind %.0f m/s %s</li>",
    .h(unlist(names[cw$spot])), .loc(cw$start, "%a %d", tz), .loc(cw$start, "%H", tz), .loc(cw$end, "%H", tz),
    cw$peak_score, cw$wave_height, cw$wave_period, cw$wind_speed, .h(gsub("_", " ", cw$wind_category))),
    collapse = ""), "</ul>") else "<p class='muted'>Nothing in the 5–6 band.</p>"

  # Region outlook: next 5 days for every region.
  pseudo <- tibble::tibble(spot = spots$spot,
    start = as.POSIXct(format(today), tz = tz) + 12 * 3600,
    end = as.POSIXct(format(today + 4), tz = tz) + 12 * 3600)
  outlook <- summarise_regions(blocks, spots, pseudo, tz)
  outlook_html <- paste(sprintf("<p><b>%s</b><br>%s</p>", .h(names(outlook)), .h(outlook)), collapse = "\n")

  # Recently alerted windows (state keeps the last alert per window).
  told <- state[!is.na(state$notified_at), ]
  told <- head(told[order(told$notified_at, decreasing = TRUE), ], 12)
  told_html <- if (nrow(told)) paste0("<ul class='told'>", paste(sprintf(
    "<li><span class='muted'>%s</span> %s · %s %s · <span class='st-%s'>%s</span></li>",
    .loc(told$notified_at, "%a %d %H:%M", tz), .h(unlist(names[told$spot])),
    .loc(told$start, "%a %d", tz), .h(told$peak_category), told$status,
    ifelse(told$status == "active", told$stage, told$status)), collapse = ""), "</ul>") else
    "<p class='muted'>No alerts yet.</p>"

  generated_iso <- format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  html <- sprintf(.PAGE_TEMPLATE,
    generated_iso, STALE_AFTER_HOURS, .loc(now, "%a %d %b %H:%M", tz),
    cards, consider_html,
    .grid(best, spots, days, tz, cuts, consider_min, gfs_days),
    outlook_html, .spot_details(best, spots, tz, cuts, consider_min), told_html)

  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  con <- file(path, open = "w", encoding = "UTF-8")
  writeLines(html, con, useBytes = FALSE)
  close(con)
  invisible(path)
}

# sprintf template: %% is a literal %. Placeholders, in order: generated ISO,
# stale hours, generated local, cards, consider, grid, outlook, spot details, alerts.
.PAGE_TEMPLATE <- "<!doctype html>
<html lang='en'>
<head>
<meta charset='utf-8'>
<meta name='viewport' content='width=device-width, initial-scale=1, viewport-fit=cover'>
<title>Surf Windows</title>
<meta name='theme-color' content='#0b1820'>
<meta name='apple-mobile-web-app-capable' content='yes'>
<meta name='apple-mobile-web-app-title' content='Surf'>
<link rel='manifest' href='manifest.webmanifest'>
<link rel='apple-touch-icon' href='apple-touch-icon.png'>
<link rel='icon' type='image/png' href='icon-192.png'>
<style>
:root{--bg:#0b1820;--panel:#12242f;--line:#1f3644;--text:#e3edf2;--muted:#8ba3b1;
--flat:#22323c;--marginal:#2d4656;--consider:#2f6a72;--good:#2f8a5a;--epic:#c27a1e;--warn:#b3402e;--accent:#6cc4d8}
@media (prefers-color-scheme: light){:root{--bg:#f4f7f9;--panel:#ffffff;--line:#d9e3e9;--text:#10232d;--muted:#5a7080;
--flat:#e7edf1;--marginal:#cfdde6;--consider:#9fd3d8;--good:#7fcf9e;--epic:#f0b35c;--warn:#c84a36;--accent:#1e7f96}}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--text);font:15px/1.45 system-ui,-apple-system,Segoe UI,Roboto,sans-serif;
padding:16px 16px calc(32px + env(safe-area-inset-bottom))}
main{max-width:680px;margin:0 auto}
h1{font-size:1.25rem;margin:0}h2{font-size:1rem;margin:28px 0 10px;color:var(--accent);text-transform:uppercase;letter-spacing:.06em}
h3{font-size:.85rem;margin:16px 0 6px;color:var(--muted);text-transform:uppercase;letter-spacing:.05em}
.muted{color:var(--muted)}.small{font-size:.8rem}
header{display:flex;justify-content:space-between;align-items:baseline;gap:12px;flex-wrap:wrap}
#fresh{font-size:.85rem;color:var(--muted)}
#stale{display:none;background:var(--warn);color:#fff;padding:10px 12px;border-radius:8px;margin-top:12px;font-weight:600}
.card{background:var(--panel);border:1px solid var(--line);border-left:5px solid var(--good);border-radius:10px;padding:12px 14px;margin:10px 0}
.card.epic{border-left-color:var(--epic)}.card:target{outline:2px solid var(--accent)}
.card-top{display:flex;justify-content:space-between;align-items:center}.spot{font-weight:700;font-size:1.05rem}
.badge{font-size:.72rem;padding:2px 8px;border-radius:99px;border:1px solid var(--line);color:var(--muted)}
.badge.confirmed{background:var(--good);color:#fff;border-color:transparent}
.badge.weakened{background:var(--warn);color:#fff;border-color:transparent}
.when{margin:2px 0}.score{font-size:1.3rem;font-weight:700}
.grid-wrap{overflow-x:auto;-webkit-overflow-scrolling:touch;border:1px solid var(--line);border-radius:10px;background:var(--panel)}
table{border-collapse:collapse;width:100%%}
.grid th,.grid td{padding:5px 0;text-align:center;font-size:.74rem;white-space:nowrap;min-width:22px}
.grid thead th{color:var(--muted);font-weight:600}.grid th.gfs,.grid td.gfs{opacity:.6}
.grid .rowname{text-align:left;font-weight:500;position:sticky;left:0;background:var(--panel);padding:5px 6px 5px 8px;
max-width:96px;overflow:hidden;text-overflow:ellipsis}
.grid .rowname a{color:var(--text);text-decoration:none}
.region-row td{text-align:left!important;color:var(--accent);font-size:.72rem!important;text-transform:uppercase;letter-spacing:.06em;padding-top:10px!important}
td.flat{background:var(--flat)}td.marginal{background:var(--marginal)}td.consider{background:var(--consider)}
td.good{background:var(--good);color:#fff;font-weight:700}td.epic{background:var(--epic);color:#fff;font-weight:700}td.na{color:var(--muted)}
.legend{display:flex;gap:10px;flex-wrap:wrap;font-size:.75rem;color:var(--muted);margin-top:8px}
.legend i{display:inline-block;width:12px;height:12px;border-radius:3px;vertical-align:-2px;margin-right:4px}
details{background:var(--panel);border:1px solid var(--line);border-radius:10px;margin:6px 0}
summary{padding:10px 12px;cursor:pointer;display:flex;justify-content:space-between;align-items:center}
.pill{font-size:.75rem;padding:2px 8px;border-radius:99px}
.pill.good{background:var(--good);color:#fff}.pill.epic{background:var(--epic);color:#fff}.pill.consider{background:var(--consider)}
.pill.marginal{background:var(--marginal)}.pill.flat,.pill.na{background:var(--flat)}
.detail{font-size:.78rem}.detail th,.detail td{padding:6px;text-align:left;border-top:1px solid var(--line);vertical-align:top}
.detail td.num{font-weight:700;text-align:center}
.detail td.good,.detail td.epic{color:#fff;font-weight:700}
.detail td.good{background:var(--good)}.detail td.epic{background:var(--epic)}.detail td.consider{background:var(--consider)}
details .detail{display:block;overflow-x:auto}
ul{padding-left:18px;margin:6px 0}li{margin:4px 0}
.st-cancelled{color:var(--warn)}.st-expired{color:var(--muted)}
footer{margin-top:32px;font-size:.75rem;color:var(--muted)}
</style>
</head>
<body><main>
<header><h1>Surf windows</h1><span id='fresh' data-generated='%s' data-stale-hours='%d'>Updated %s</span></header>
<div id='stale'></div>

<h2>Windows</h2>
%s

<h2>Worth a look</h2>
<p class='muted small'>Scores 5–6: below Good, but maybe worth a drive in the right mood. Never pushed as alerts.</p>
%s

<h2>Next 10 days</h2>
%s
<div class='legend'><span><i style='background:var(--good)'></i>Good 6+</span><span><i style='background:var(--epic)'></i>Epic 8.5+</span>
<span><i style='background:var(--consider)'></i>Worth a look 5–6</span><span><i style='background:var(--marginal)'></i>Marginal</span>
<span>Faded columns: GFS (low confidence)</span></div>

<h2>Outlook</h2>
%s

<h2>Spots</h2>
%s

<h2>Recent alerts</h2>
%s

<footer>Open-Meteo (DWD EWAM / ICON-EU, NOAA GFS). Directional guide, not a forecast to bet a trip on. Times Europe/Copenhagen.</footer>
</main>
<script>
(function(){
  var el=document.getElementById('fresh');
  var gen=new Date(el.dataset.generated), hrs=(Date.now()-gen.getTime())/36e5;
  var ago=hrs<1?Math.round(hrs*60)+' min ago':Math.round(hrs)+' h ago';
  el.textContent='Updated '+ago;
  if(hrs>+el.dataset.staleHours){
    var s=document.getElementById('stale');
    s.textContent='Stale: last update '+ago+'. The pipeline may be failing; check GitHub Actions.';
    s.style.display='block';
  }
})();
</script>
</body></html>"

# test_score_block.R
# Regression checks for score_block.R, built from real forecast cases.
#   Rscript tests/test_score_block.R

suppressMessages(source("R/score_block.R"))
tiers <- load_tiers()
stopifnot(
  "tiers.yaml did not parse fully" = !is.null(tiers$groundswell_classification$min_swell_share),
  "direction_exposure missing" = !is.null(tiers$direction_exposure$zero_deg)
)

# Build n identical hourly rows (so the fetch gate sees a steady history)
# and score the last one. Defaults: Molle havn, waves head-on.
blk <- function(wave_h, wave_p, wave_dir = 30, swell_h = 0, swell_p = 0, swell_dir = wave_dir,
                wdir = 215, wspd = 7, tier = "B_mixed", facing = 30, arc = c(180, 250), n = 12,
                full_deg = NA, zero_deg = NA, cfg = tiers) {
  h <- data.frame(
    spot = "test", tier = tier,
    datetime = as.POSIXct("2026-10-05", tz = "UTC") + (0:(n - 1)) * 3600,
    wave_height = wave_h, wave_period = wave_p, wave_direction = wave_dir,
    swell_wave_height = swell_h, swell_wave_period = swell_p, swell_wave_direction = swell_dir,
    wind_speed_10m = wspd, wind_direction_10m = wdir,
    facing_deg = facing, offshore_arc_min = arc[1], offshore_arc_max = arc[2],
    exposure_full_deg = full_deg, exposure_zero_deg = zero_deg
  )
  s <- score_spot(h, cfg)
  s[nrow(s), ]
}

# 1. Molle 2026-10-05 18:00: trace swell 0.06 m @ 8.6 s under 1.22 m @ 5.25 s,
#    offshore. Was groundswell / 6.1. Now windsea, gate skipped (offshore): 5.05*1.1
b <- blk(1.22, 5.25, swell_h = 0.06, swell_p = 8.6)
stopifnot(b$block_type == "windsea_block", b$score == 5.6, b$category == "Marginal")

# 2. Same day midday: 1.34 m @ 5.3 s offshore. Gate used to halve to 2.8.
stopifnot(blk(1.34, 5.3)$score == 5.6)

# 3. Fetch gate: disabled by default (2026-10-02). When re-enabled it still
#    halves onshore windsea that hasn't built (onshore only in final 2 of 12 h).
stopifnot(isFALSE(tiers$fetch_gate$enabled))
gate_case <- function(cfg) {
  h <- data.frame(spot = "test", tier = "B_mixed",
    datetime = as.POSIXct("2026-10-05", tz = "UTC") + (0:11) * 3600,
    wave_height = 1.34, wave_period = 5.3, wave_direction = 30,
    swell_wave_height = 0, swell_wave_period = 0, swell_wave_direction = 30,
    wind_speed_10m = 7, wind_direction_10m = c(rep(215, 10), 30, 30),
    facing_deg = 30, offshore_arc_min = 180, offshore_arc_max = 250)
  s <- score_spot(h, cfg); s[12, ]
}
tiers_gate <- tiers; tiers_gate$fetch_gate$enabled <- TRUE
stopifnot(gate_case(tiers)$score == 4.5)        # 5.05 * 0.9 onshore_light, no halving
stopifnot(gate_case(tiers_gate)$score == 2.3)   # halved
stopifnot(gate_case(tiers_gate)$limiting_factor %in% c("wind_onshore_light", "fetch_not_built"))

# 4. Real groundswell under chop via the swell component (blended period short)
b <- blk(1.2, 7, swell_h = 0.8, swell_p = 10)
stopifnot(b$block_type == "groundswell_block", b$wind_modifier == 1.2)

# 5. Swell share boundary: 0.6 of 1.2 m is exactly 50% -> groundswell; 0.59 -> not
stopifnot(blk(1.2, 7, swell_h = 0.6, swell_p = 10)$block_type == "groundswell_block")
stopifnot(blk(1.2, 7, swell_h = 0.59, swell_p = 10)$block_type == "windsea_block")

# 6. GFS reports swell as 0: long blended period alone makes it groundswell
stopifnot(blk(1.5, 11, tier = "A_groundswell", facing = 280, wave_dir = 280, arc = c(80, 160), wdir = 120)$block_type == "groundswell_block")

# 7. Asa north, Sat 10 Oct (real): sea from 202 deg, spot faces 350 -> 148 deg off.
#    Was Epic 8.7. Must be zero, limited by direction.
b <- blk(1.92, 6.55, wave_dir = 202, swell_h = 0.1, swell_p = 5, swell_dir = 307,
         wdir = 214, wspd = 9.87, facing = 350, arc = c(140, 220))
stopifnot(b$exposure == 0, b$score == 0, b$limiting_factor == "swell_direction")
stopifnot(b$wind_category == "offshore_strong")

# 8. Asa south, same hour: faces 170, 32 deg off -> exposed; strong onshore limits it.
b <- blk(1.92, 6.55, wave_dir = 202, swell_h = 0.1, swell_p = 5, swell_dir = 307,
         wdir = 214, wspd = 9.87, facing = 170, arc = c(320, 40))
stopifnot(b$exposure > 0.95, b$wind_category == "onshore_strong", b$limiting_factor == "wind_onshore_strong")
stopifnot(abs(b$score - 4.6) < 0.05)

# 9. Hanstholm point, Sat 10 Oct (real): 2.0 m @ 8.95 s from 284, faces 0 (76 deg off),
#    11.5 m/s offshore. Was Epic 8.7. Now heavily cut by direction.
b <- blk(2.0, 8.95, wave_dir = 284, tier = "A_groundswell", facing = 0, arc = c(150, 230),
         wdir = 227, wspd = 11.5)
stopifnot(b$wind_category == "offshore_strong", abs(b$exposure - 0.34) < 0.01, b$score < 3)
stopifnot(b$limiting_factor == "swell_direction")

# 10. Height fade below the "poor" cutoff (tier A poor = 0.8 m):
#     0.6 m @ 10 s clean groundswell stays Good; 0.1 m @ 10 s is Flat.
a <- function(h) blk(h, 10, wave_dir = 280, swell_h = h, swell_p = 10, tier = "A_groundswell",
                     facing = 280, arc = c(80, 160), wdir = 120, wspd = 4)
stopifnot(a(0.6)$category == "Good", a(0.6)$block_type == "groundswell_block")
stopifnot(a(0.1)$category == "Flat")

# 11. Hanstholm point per-spot override (works in W/WNW storm swell).
#     Same 284 deg sea as case 9 gets full exposure with full_deg 80 / zero_deg 120,
#     but a SW sea (230 deg, 130 off) is still zeroed.
b <- blk(2.0, 8.95, wave_dir = 284, tier = "A_groundswell", facing = 0, arc = c(150, 230),
         wdir = 200, wspd = 6, full_deg = 80, zero_deg = 120)
stopifnot(b$exposure == 1, b$score >= 6)  # 7.875 * 1.1 offshore = 8.7
stopifnot(blk(2.0, 8.95, wave_dir = 230, tier = "A_groundswell", facing = 0, arc = c(150, 230),
              wdir = 200, wspd = 6, full_deg = 80, zero_deg = 120)$exposure == 0)

cat("All score_block tests passed.\n")

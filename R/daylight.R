# daylight.R
# Sunrise/sunset and civil dawn/dusk from the NOAA solar-position
# approximation (accurate to ~1-2 min at these latitudes, far better than
# the 1-hour block resolution it feeds). Written out by hand rather than
# using suncalc to keep the GitHub Actions dependency chain short.
#
# All inputs and outputs are UTC. Pass true-UTC datetimes only, so
# fetch_forecast.R must request timezone = "GMT" (see note there).

# zenith_deg: 90.833 = sunrise/sunset (refraction-corrected), 96 = civil
# dawn/dusk (enough light to see the lineup).
.LIGHT_ZENITH <- c(sunrise = 90.833, civil = 96)

sun_times <- function(date, lat, lon, light = c("civil", "sunrise")) {
  light <- match.arg(light)
  zenith <- .LIGHT_ZENITH[[light]]

  doy <- as.integer(format(date, "%j"))
  gamma <- 2 * pi / 365 * (doy - 1) # fractional year at ~noon

  eqtime <- 229.18 *
    (0.000075 +
      0.001868 * cos(gamma) -
      0.032077 * sin(gamma) -
      0.014615 * cos(2 * gamma) -
      0.040849 * sin(2 * gamma))
  decl <- 0.006918 -
    0.399912 * cos(gamma) +
    0.070257 * sin(gamma) -
    0.006758 * cos(2 * gamma) +
    0.000907 * sin(2 * gamma) -
    0.002697 * cos(3 * gamma) +
    0.00148 * sin(3 * gamma)

  lat_r <- lat * pi / 180
  cos_ha <- cos(zenith * pi / 180) / (cos(lat_r) * cos(decl)) -
    tan(lat_r) * tan(decl)
  # Clamp for polar day/night; irrelevant at 55-57N but avoids NaN.
  ha <- acos(pmin(pmax(cos_ha, -1), 1)) * 180 / pi

  midnight <- as.POSIXct(format(date, "%Y-%m-%d"), tz = "UTC")
  data.frame(
    date = date,
    dawn = midnight + (720 - 4 * (lon + ha) - eqtime) * 60,
    dusk = midnight + (720 - 4 * (lon - ha) - eqtime) * 60
  )
}

# TRUE where the hourly block at `datetime` is surfable light. A block's
# value is an instantaneous forecast for that hour, so it stands in for
# roughly +/-30 min; pad_min widens the light window by that much each side.
is_daylight <- function(
  datetime,
  lat,
  lon,
  light = "civil",
  pad_min = 30
) {
  stopifnot(inherits(datetime, "POSIXct"))
  st <- sun_times(as.Date(datetime, tz = "UTC"), lat, lon, light)
  pad <- pad_min * 60
  datetime >= st$dawn - pad & datetime <= st$dusk + pad
}

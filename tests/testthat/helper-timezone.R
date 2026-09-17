# R silently uses UTC for time zones it doesn't know (see `?timezones`), and
# zones like "CET" are not available everywhere (e.g. Ubuntu 26.04 ships them
# in the optional tzdata-legacy package), so we skip tests that rely on them
skip_if_tz_unavailable <- function(tz) {
  # all the zones we test are ahead of UTC on this date
  offset <- suppressWarnings(
    format(as.POSIXct("2022-01-01", tz = "UTC"), tz = tz, format = "%z")
  )
  skip_if(offset == "+0000", sprintf("time zone '%s' is not available", tz))
}

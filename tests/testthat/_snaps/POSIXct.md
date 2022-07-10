# POSIXct

    Code
      construct(.leap.seconds[1:4])
    Output
      as.POSIXct(c("1972-07-01", "1973-01-01", "1974-01-01", "1975-01-01"), tz = "GMT")
    Code
      construct(sys_time_1970)
    Output
      as.POSIXct("1970-01-01 01:00:00") |>
        structure(tzone = NULL)
    Code
      construct(as.POSIXct("2022-01-01 01:00:00", tz = "UTC"))
    Output
      as.POSIXct("2022-01-01 01:00:00", tz = "UTC")
    Code
      construct(as.POSIXct("2022-01-01 01:00:00.1", tz = "GMT"))
    Output
      as.POSIXct("2022-01-01 01:00:00.1", tz = "GMT")


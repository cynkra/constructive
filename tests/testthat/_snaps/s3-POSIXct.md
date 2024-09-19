# POSIXct post (incl) 4.3

    Code
      construct(.leap.seconds[1:4])
    Output
      as.POSIXct(c("1972-07-01", "1973-01-01", "1974-01-01", "1975-01-01"), tz = "GMT")
    Code
      construct(sys_time_1970)
    Output
      as.POSIXct("1970-01-01") |>
        structure(tzone = NULL)
    Code
      construct(as.POSIXct("2022-01-01 01:00:00", tz = "UTC"))
    Output
      as.POSIXct("2022-01-01 01:00:00", tz = "UTC")
    Code
      construct(as.POSIXct("2022-01-01 01:00:00", tz = "UTC"), opts_POSIXct(
        ".POSIXct"))
    Output
      .POSIXct(1640998800, tz = "UTC")
    Code
      construct(as.POSIXct("2022-01-01 01:00:00", tz = "UTC"), opts_POSIXct(
        "as_datetime"))
    Output
      lubridate::as_datetime("2022-01-01 01:00:00")
    Code
      construct(as.POSIXct("2022-01-01 01:00:00", tz = "CET"), opts_POSIXct(
        "as_datetime"))
    Output
      lubridate::as_datetime("2022-01-01 01:00:00", tz = "CET")
    Code
      construct(as.POSIXct("2022-01-01 01:00:00", tz = "UTC"), opts_POSIXct(
        "as.POSIXct"))
    Output
      as.POSIXct("2022-01-01 01:00:00", tz = "UTC")
    Code
      construct(as.POSIXct("2022-01-01 01:00:00", tz = "UTC"), opts_POSIXct(
        "as.POSIXct.numeric"))
    Output
      as.POSIXct(1640998800, tz = "UTC", origin = "1970-01-01")
    Code
      construct(as.POSIXct("2022-01-01 01:00:00", tz = "CET"), opts_POSIXct(
        "as.POSIXct.numeric", origin = "2000-01-01"))
    Output
      as.POSIXct(694310400, tz = "CET", origin = "2000-01-01")
    Code
      construct(as.POSIXct("2022-01-01 01:00:00", tz = "UTC"), opts_POSIXct(
        "as_datetime.numeric"))
    Output
      lubridate::as_datetime(1640998800)
    Code
      construct(as.POSIXct("2022-01-01 01:00:00", tz = "CET"), opts_POSIXct(
        "as_datetime.numeric", origin = "2000-01-01"))
    Output
      lubridate::as_datetime(694310400, tz = "CET", origin = "2000-01-01")
    Code
      construct(as.POSIXct("2022-01-01 01:00:00", tz = "CET"), opts_POSIXct("next"))
    Output
      1640995200 |>
        structure(class = c("POSIXct", "POSIXt"), tzone = "CET")
    Code
      construct(as.POSIXct("2022-01-01 01:00:00", tz = "CET"), opts_POSIXct("atomic"))
    Output
      1640995200 |>
        structure(class = c("POSIXct", "POSIXt"), tzone = "CET")
    Code
      construct(as.POSIXct("2022-01-01 01:00:00.1", tz = "GMT"))
    Output
      as.POSIXct("2022-01-01 01:00:00.1", tz = "GMT")
    Code
      construct(as.POSIXct(c("2022-01-01 01:00:00.1", NA), tz = "GMT"))
    Output
      as.POSIXct(c("2022-01-01 01:00:00.1", NA), tz = "GMT")
    Code
      construct(as.POSIXct(c("2024-07-22 13:25:22.868974",
        "2024-07-22 13:25:22.868976"), tz = "UTC"))
    Output
      as.POSIXct(c("2024-07-22 13:25:22.868974", "2024-07-22 13:25:22.868976"), tz = "UTC")


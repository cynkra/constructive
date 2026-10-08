# Interval

    Code
      start <- as.POSIXct(c("2020-01-01 00:00:00", NA, "2020-06-01 12:30:00"), tz = "Europe/Paris")
      end <- as.POSIXct(c("2021-01-01 00:00:00", "2020-05-01 00:00:00",
        "2020-06-01 12:30:10.5"), tz = "Europe/Paris")
      construct(lubridate::interval(start, end))
    Output
      lubridate::interval(
        as.POSIXct(c("2020-01-01 00:00:00", NA, "2020-06-01 12:30:00"), tz = "Europe/Paris"),
        as.POSIXct(c("2021-01-01 00:00:00", NA, "2020-06-01 12:30:10.5"), tz = "Europe/Paris")
      )
    Code
      construct(lubridate::interval(start, end, tzone = "UTC"))
    Output
      lubridate::interval(
        as.POSIXct(c("2019-12-31 23:00:00", NA, "2020-06-01 10:30:00"), tz = "UTC"),
        as.POSIXct(c("2020-12-31 23:00:00", NA, "2020-06-01 10:30:10.5"), tz = "UTC")
      )
    Code
      construct(lubridate::interval(end, start))
    Output
      lubridate::interval(
        as.POSIXct(
          c("2021-01-01 00:00:00", "2020-05-01 00:00:00", "2020-06-01 12:30:10.5"),
          tz = "Europe/Paris"
        ),
        as.POSIXct(c("2020-01-01 00:00:00", NA, "2020-06-01 12:30:00"), tz = "Europe/Paris")
      )
    Code
      construct(lubridate::interval())
    Output
      lubridate::interval()
    Code
      construct(lubridate::interval(tzone = "Europe/Paris"))
    Output
      lubridate::interval(tzone = "Europe/Paris")
    Code
      construct(structure(lubridate::interval(start, end), foo = "bar"))
    Output
      lubridate::interval(
        as.POSIXct(c("2020-01-01 00:00:00", NA, "2020-06-01 12:30:00"), tz = "Europe/Paris"),
        as.POSIXct(c("2021-01-01 00:00:00", NA, "2020-06-01 12:30:10.5"), tz = "Europe/Paris")
      ) |>
        structure(foo = "bar")
    Code
      construct(methods::new("Interval", 1, start = as.POSIXct("2020-01-01", tz = "UTC"),
      tzone = "Europe/Paris"))
    Output
      1 |>
        structure(
          start = as.POSIXct("2020-01-01", tz = "UTC"),
          tzone = "Europe/Paris",
          class = "Interval" |>
            structure(package = "lubridate")
        ) |>
        asS4()
    Code
      construct(lubridate::interval(start, end), opts_Interval("interval"))
    Output
      lubridate::interval(
        as.POSIXct(c("2020-01-01 00:00:00", NA, "2020-06-01 12:30:00"), tz = "Europe/Paris"),
        as.POSIXct(c("2021-01-01 00:00:00", NA, "2020-06-01 12:30:10.5"), tz = "Europe/Paris")
      )
    Code
      construct(lubridate::interval(start, end), opts_Interval("next"))
    Output
      c(31622400, NA, 10.5) |>
        structure(
          start = as.POSIXct(c("2020-01-01 00:00:00", NA, "2020-06-01 12:30:00"), tz = "Europe/Paris"),
          tzone = "Europe/Paris",
          class = "Interval" |>
            structure(package = "lubridate")
        ) |>
        asS4()


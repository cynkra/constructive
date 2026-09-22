test_that("Interval", {
  skip_if_not_installed("lubridate")
  expect_snapshot({
    start <- as.POSIXct(c("2020-01-01 00:00:00", NA, "2020-06-01 12:30:00"), tz = "Europe/Paris")
    end <- as.POSIXct(c("2021-01-01 00:00:00", "2020-05-01 00:00:00", "2020-06-01 12:30:10.5"), tz = "Europe/Paris")
    construct(lubridate::interval(start, end))
    construct(lubridate::interval(start, end, tzone = "UTC"))
    construct(lubridate::interval(end, start))
    construct(lubridate::interval())
    construct(lubridate::interval(tzone = "Europe/Paris"))
    construct(structure(lubridate::interval(start, end), foo = "bar"))
    # tzone slot inconsistent with start, can't use `interval()`
    construct(methods::new(
      "Interval", 1,
      start = as.POSIXct("2020-01-01", tz = "UTC"),
      tzone = "Europe/Paris"
    ))
    construct(lubridate::interval(start, end), opts_Interval("interval"))
    construct(lubridate::interval(start, end), opts_Interval("next"))
  })
})

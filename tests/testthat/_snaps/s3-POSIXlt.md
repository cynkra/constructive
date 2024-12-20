# POSIXlt-all-versions

    Code
      construct(as.POSIXlt(.leap.seconds[1:4]))
    Output
      as.POSIXlt(c("1972-07-01", "1973-01-01", "1974-01-01", "1975-01-01"), tz = "GMT")
    Code
      construct(as.POSIXlt(.leap.seconds[1:4]))
    Output
      as.POSIXlt(c("1972-07-01", "1973-01-01", "1974-01-01", "1975-01-01"), tz = "GMT")
    Code
      construct(as.POSIXlt(sys_time_1970))
    Output
      as.POSIXlt("1970-01-01", tz = "UTC")
    Code
      construct(as.POSIXlt("2022-01-01 01:00:00", tz = "UTC"))
    Output
      as.POSIXlt("2022-01-01 01:00:00", tz = "UTC")
    Code
      construct(as.POSIXlt("2022-01-01 01:00:00.1", tz = "GMT"))
    Output
      as.POSIXlt("2022-01-01 01:00:00.1", tz = "GMT")
    Code
      construct(as.POSIXlt(c("2022-01-01 01:00:00.1", NA), tz = "GMT"))
    Output
      as.POSIXlt(c("2022-01-01 01:00:00.1", NA), tz = "GMT")
    Code
      construct(as.POSIXlt(c("2024-07-22 13:25:22.868974",
        "2024-07-22 13:25:22.868976"), tz = "UTC"))
    Output
      as.POSIXlt(c("2024-07-22 13:25:22.868974", "2024-07-22 13:25:22.868976"), tz = "UTC")

# POSIXlt-from-4.3

    Code
      construct(as.POSIXlt(.leap.seconds[1:4]), opts_POSIXlt("next"))
    Output
      list(
        sec = numeric(4),
        min = integer(4),
        hour = integer(4),
        mday = rep(1L, 4L),
        mon = c(6L, 0L, 0L, 0L),
        year = 72:75,
        wday = c(6L, 1L, 2L, 3L),
        yday = c(182L, 0L, 0L, 0L),
        isdst = integer(4),
        zone = rep("GMT", 4L),
        gmtoff = integer(4)
      ) |>
        structure(class = c("POSIXlt", "POSIXt"), tzone = "GMT", balanced = TRUE)
    Code
      construct(as.POSIXlt(.leap.seconds[1:4]), opts_POSIXlt("list"))
    Output
      list(
        sec = numeric(4),
        min = integer(4),
        hour = integer(4),
        mday = rep(1L, 4L),
        mon = c(6L, 0L, 0L, 0L),
        year = 72:75,
        wday = c(6L, 1L, 2L, 3L),
        yday = c(182L, 0L, 0L, 0L),
        isdst = integer(4),
        zone = rep("GMT", 4L),
        gmtoff = integer(4)
      ) |>
        structure(class = c("POSIXlt", "POSIXt"), tzone = "GMT", balanced = TRUE)

---

    Code
      construct(as.POSIXlt(.leap.seconds[1:4]))
    Output
      as.POSIXlt(c("1972-07-01", "1973-01-01", "1974-01-01", "1975-01-01"), tz = "GMT")
    Code
      construct(as.POSIXlt(sys_time_1970))
    Output
      as.POSIXlt(as.POSIXct("1970-01-01 01:00:00", tz = "CET"))
    Code
      construct(as.POSIXlt(.leap.seconds[1:4]), opts_POSIXlt("next"))
    Output
      list(
        sec = numeric(4),
        min = integer(4),
        hour = integer(4),
        mday = rep(1L, 4L),
        mon = c(6L, 0L, 0L, 0L),
        year = 72:75,
        wday = c(6L, 1L, 2L, 3L),
        yday = c(182L, 0L, 0L, 0L),
        isdst = integer(4),
        zone = rep("GMT", 4L),
        gmtoff = integer(4)
      ) |>
        structure(class = c("POSIXlt", "POSIXt"), tzone = "GMT", balanced = TRUE)
    Code
      construct(as.POSIXlt(.leap.seconds[1:4]), opts_POSIXlt("list"))
    Output
      list(
        sec = numeric(4),
        min = integer(4),
        hour = integer(4),
        mday = rep(1L, 4L),
        mon = c(6L, 0L, 0L, 0L),
        year = 72:75,
        wday = c(6L, 1L, 2L, 3L),
        yday = c(182L, 0L, 0L, 0L),
        isdst = integer(4),
        zone = rep("GMT", 4L),
        gmtoff = integer(4)
      ) |>
        structure(class = c("POSIXlt", "POSIXt"), tzone = "GMT", balanced = TRUE)


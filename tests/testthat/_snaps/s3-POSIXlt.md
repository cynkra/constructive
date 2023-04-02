# POSIXlt

    Code
      construct(as.POSIXlt(.leap.seconds[1:4]))
    Output
      as.POSIXlt(c("1972-07-01", "1973-01-01", "1974-01-01", "1975-01-01"), tz = "GMT")
    Code
      construct(as.POSIXlt(sys_time_1970))
    Output
      as.POSIXlt(as.POSIXct("1970-01-01 01:00:00"))
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


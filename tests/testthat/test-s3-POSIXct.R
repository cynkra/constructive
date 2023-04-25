test_that("POSIXct", {
  sys_time_1970 <- Sys.time()
  sys_time_1970[[1]] <- 0
  expect_snapshot({
    # ordered
    construct(.leap.seconds[1:4])
    construct(sys_time_1970)
    construct(as.POSIXct("2022-01-01 01:00:00", tz = "UTC"))
    construct(as.POSIXct("2022-01-01 01:00:00", tz = "UTC"), opts_POSIXct(".POSIXct"))
    construct(as.POSIXct("2022-01-01 01:00:00", tz = "UTC"), opts_POSIXct("as_datetime"))
    construct(as.POSIXct("2022-01-01 01:00:00", tz = "CET"), opts_POSIXct("as_datetime"))
    construct(as.POSIXct("2022-01-01 01:00:00", tz = "UTC"), opts_POSIXct("as.POSIXct"))
    construct(as.POSIXct("2022-01-01 01:00:00", tz = "UTC"), opts_POSIXct("as.POSIXct.numeric"))
    construct(as.POSIXct("2022-01-01 01:00:00", tz = "CET"), opts_POSIXct("as.POSIXct.numeric", origin = "2000-01-01"))
    construct(as.POSIXct("2022-01-01 01:00:00", tz = "UTC"), opts_POSIXct("as_datetime.numeric"))
    construct(as.POSIXct("2022-01-01 01:00:00", tz = "CET"), opts_POSIXct("as_datetime.numeric", origin = "2000-01-01"))
    construct(as.POSIXct("2022-01-01 01:00:00", tz = "CET"), opts_POSIXct("next"))
    construct(as.POSIXct("2022-01-01 01:00:00", tz = "CET"), opts_POSIXct("atomic"))
    construct(as.POSIXct("2022-01-01 01:00:00.1", tz = "GMT"))
    construct(as.POSIXct(c("2022-01-01 01:00:00.1", NA), tz = "GMT"))
  })
})

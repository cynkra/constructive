test_that("POSIXlt", {
  sys_time_1970 <- Sys.time()
  sys_time_1970[[1]] <- 0
  expect_snapshot({
    # ordered
    construct(as.POSIXlt(.leap.seconds[1:4]))
    construct(as.POSIXlt(.leap.seconds[1:4]), opts_POSIXlt("next"))
    construct(as.POSIXlt(.leap.seconds[1:4]), opts_POSIXlt("list"))
    construct(as.POSIXlt(.leap.seconds[1:4]))
    construct(as.POSIXlt(sys_time_1970))
    construct(as.POSIXlt("2022-01-01 01:00:00", tz = "UTC"))
    construct(as.POSIXlt("2022-01-01 01:00:00.1", tz = "GMT"))
    construct(as.POSIXlt(c("2022-01-01 01:00:00.1", NA), tz = "GMT"))
  })
})

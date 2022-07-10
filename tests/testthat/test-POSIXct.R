test_that("POSIXct", {
  sys_time_1970 <- Sys.time()
  sys_time_1970[[1]] <- 0
  expect_snapshot({
    # ordered
    construct(.leap.seconds[1:4])
    construct(sys_time_1970)
    construct(as.POSIXct("2022-01-01 01:00:00", tz = "UTC"))
    construct(as.POSIXct("2022-01-01 01:00:00.1", tz = "GMT"))
  })
})

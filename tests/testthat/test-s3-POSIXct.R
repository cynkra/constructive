test_that("POSIXct pre (excl) 4.3", {
  skip_if(with_versions(R >= "4.3"))


  # For stability
  .leap.seconds <- as.POSIXct(
    c(
      "1972-07-01", "1973-01-01", "1974-01-01", "1975-01-01", "1976-01-01",
      "1977-01-01", "1978-01-01", "1979-01-01", "1980-01-01", "1981-07-01",
      "1982-07-01", "1983-07-01", "1985-07-01", "1988-01-01", "1990-01-01",
      "1991-01-01", "1992-07-01", "1993-07-01", "1994-07-01", "1996-01-01",
      "1997-07-01", "1999-01-01", "2006-01-01", "2009-01-01", "2012-07-01",
      "2015-07-01", "2017-01-01"
    ),
    tz = "GMT"
  )

  withr::local_timezone("UTC")
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
    construct(as.POSIXct(c("2024-07-22 13:25:22.868974", "2024-07-22 13:25:22.868976"), tz = "UTC"))
  })
})

test_that("POSIXct post (incl) 4.3", {
  skip_if(with_versions(R < "4.3"))


  # For stability
  .leap.seconds <- as.POSIXct(
    c(
      "1972-07-01", "1973-01-01", "1974-01-01", "1975-01-01", "1976-01-01",
      "1977-01-01", "1978-01-01", "1979-01-01", "1980-01-01", "1981-07-01",
      "1982-07-01", "1983-07-01", "1985-07-01", "1988-01-01", "1990-01-01",
      "1991-01-01", "1992-07-01", "1993-07-01", "1994-07-01", "1996-01-01",
      "1997-07-01", "1999-01-01", "2006-01-01", "2009-01-01", "2012-07-01",
      "2015-07-01", "2017-01-01"
    ),
    tz = "GMT"
  )

  withr::local_timezone("UTC")
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
    construct(as.POSIXct(c("2024-07-22 13:25:22.868974", "2024-07-22 13:25:22.868976"), tz = "UTC"))
  })
})

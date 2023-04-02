test_that("Date", {
  expect_snapshot({
    # one line
    construct(as.Date(.leap.seconds[1:5]))
    construct(as.Date(.leap.seconds[1:5]), opts_Date("as_date"))
    construct(as.Date(.leap.seconds[1:5]), opts_Date("date"))
    construct(as.Date(.leap.seconds[1:5]), opts_Date("new_date"))
    construct(as.Date(.leap.seconds[1:5]), opts_Date("as.Date.numeric"))
    construct(as.Date(.leap.seconds[1:5]), opts_Date("as_date.numeric"))
    construct(as.Date(.leap.seconds[1:5]), opts_Date("as.Date.numeric", origin = "2000-01-01"))
    construct(as.Date(.leap.seconds[1:5]), opts_Date("as_date.numeric", origin = "2000-01-01"))
    # multiline,
    construct(as.Date(.leap.seconds[1:10]))
    # handle infinite dates
    dates <- as.Date(.leap.seconds[1:5])
    dates[1] <- Inf
    dates[2] <- -Inf
    construct(dates)
    construct(dates, opts_Date("as_date"))
    construct(dates, opts_Date("date"))
    construct(dates, opts_Date("new_date"))
    construct(dates, opts_Date(origin = "2000-01-01"))
    construct(dates, opts_Date("as_date", origin = "2000-01-01"))
    construct(dates, opts_Date("date", origin = "2000-01-01"))
  })
})

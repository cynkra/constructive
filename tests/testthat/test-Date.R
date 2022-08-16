test_that("Date", {
  expect_snapshot({
    # one line
    construct(as.Date(.leap.seconds[1:5]))
    # multiline,
    construct(as.Date(.leap.seconds[1:10]))
    # handle infinite dates
    dates <- as.Date(.leap.seconds[1:5])
    dates[1] <- Inf
    dates[2] <- -Inf
    construct(dates)
  })
})

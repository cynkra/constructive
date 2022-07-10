test_that("Date", {
  expect_snapshot({
    # one line
    construct(as.Date(.leap.seconds[1:5]))
    # multiline,
    construct(as.Date(.leap.seconds[1:10]))
  })
})

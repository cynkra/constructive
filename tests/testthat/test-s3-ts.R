test_that("ts", {
  expect_snapshot({
    construct(ts(1:10, frequency = 4, start = c(1959, 2)))
    construct(ts(1:10, frequency = 4, start = c(1959, 2)), opts_ts("next"))
    construct(ts(1:10, frequency = 4, start = c(1959, 2)), opts_ts("atomic"))
    construct(ts(1:10, frequency = 7, start = c(12, 2)))
  })
})

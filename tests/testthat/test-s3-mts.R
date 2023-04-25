test_that("mts", {
  expect_snapshot({
    construct(ts(matrix(1:9, 3, 3), start = c(1961, 1), frequency = 12))
    construct(ts(matrix(1:9, 3, 3), start = c(1961, 1), frequency = 12), opts_mts("next"))
    construct(ts(matrix(1:9, 3, 3), start = c(1961, 1), frequency = 12), opts_mts("atomic"))
  })
})

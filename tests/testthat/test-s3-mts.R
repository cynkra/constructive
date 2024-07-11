test_that("mts from 4.3", {
  skip_if(with_versions(R < "4.3"))
  expect_snapshot({
    construct(ts(matrix(1:9, 3, 3), start = c(1961, 1), frequency = 12))
    construct(ts(matrix(1:9, 3, 3), start = c(1961, 1), frequency = 12), opts_mts("next"))
    construct(ts(matrix(1:9, 3, 3), start = c(1961, 1), frequency = 12), opts_mts("atomic"))
  })
})

test_that("mts before 4.3", {
  skip_if(with_versions(R >= "4.3"))
  expect_snapshot({
    construct(ts(matrix(1:9, 3, 3), start = c(1961, 1), frequency = 12))
    construct(ts(matrix(1:9, 3, 3), start = c(1961, 1), frequency = 12), opts_mts("next"))
    construct(ts(matrix(1:9, 3, 3), start = c(1961, 1), frequency = 12), opts_mts("atomic"))
  })
})

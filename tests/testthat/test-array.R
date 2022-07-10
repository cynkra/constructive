test_that("array", {
  expect_snapshot(
    construct(as.array(month.abb))
  )

  expect_snapshot(
    construct(array(1:3, c(2,4)))
  )
})

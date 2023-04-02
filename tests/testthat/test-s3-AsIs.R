test_that("AsIs", {
  expect_snapshot(
    construct(I(month.abb))
  )

  expect_snapshot(
    construct(I(head(cars,2)))
  )
})

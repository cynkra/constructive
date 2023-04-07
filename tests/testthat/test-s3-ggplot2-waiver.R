test_that("waiver", {
  expect_snapshot(
    construct(ggplot2::waiver())
  )
})

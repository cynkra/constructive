test_that("theme", {
  expect_snapshot(
    construct(ggplot2::theme_bw())
  )
})

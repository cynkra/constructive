test_that("Scale", {
  expect_snapshot(
    construct(ggplot2::scale_alpha(), check = FALSE)
  )
})

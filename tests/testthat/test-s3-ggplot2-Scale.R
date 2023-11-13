test_that("Scale", {
  expect_pipe_snapshot(
    construct(ggplot2::scale_alpha())
  )
})

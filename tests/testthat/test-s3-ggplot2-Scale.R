test_that("Scale", {
  expect_construct(ggplot2::ggplot() + ggplot2::scale_alpha())
  expect_construct(ggplot2::ggplot() + ggplot2::scale_alpha(range = c(0, 1)))
})

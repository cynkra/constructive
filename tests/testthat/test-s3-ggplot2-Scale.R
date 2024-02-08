test_that("Scale", {
  expect_faithful_ggplot_construction(ggplot2::ggplot() + ggplot2::scale_alpha())
})

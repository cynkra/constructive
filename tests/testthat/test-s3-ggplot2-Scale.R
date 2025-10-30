test_that("Scale", {
  skip_if(with_versions(R < "4.4"))
  expect_construct(ggplot2::ggplot() + ggplot2::scale_alpha())
  expect_construct(ggplot2::ggplot() + ggplot2::scale_alpha(range = c(0, 1)))
})

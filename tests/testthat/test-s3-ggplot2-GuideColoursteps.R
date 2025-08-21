test_that("GuideColourbar", {
  skip_if_not_installed("ggplot2")
  skip_if(with_versions(ggplot2 <= "3.5.2"))
  expect_construct(ggplot2::guide_colorsteps(even.steps = FALSE))
  expect_construct(ggplot2::guide_colorsteps(show.limits = TRUE))
})

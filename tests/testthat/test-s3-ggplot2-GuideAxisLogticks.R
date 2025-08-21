test_that("GuideAxisLogticks", {
  skip_if_not_installed("ggplot2")
  skip_if(with_versions(ggplot2 <= "3.5.2"))
  # examples from ?guide_axis_logticks
  expect_construct(ggplot2::guide_axis_logticks(prescale.base = 2))
  expect_construct(ggplot2::guide_axis_logticks(negative.small = 1))
})

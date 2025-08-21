test_that("GuideAxisStack", {
  skip_if_not_installed("ggplot2")
  skip_if(with_versions(ggplot2 <= "3.5.2"))
  expect_construct(
    ggplot2::guide_axis_stack(first = "axis", ggplot2::guide_axis(cap = "both"))
  )
})

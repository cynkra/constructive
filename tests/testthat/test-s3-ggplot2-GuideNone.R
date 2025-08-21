test_that("GuideNone", {
  skip_if_not_installed("ggplot2")
  skip_if(with_versions(ggplot2 <= "3.5.2"))
  expect_construct(ggplot2::guide_none())
  expect_construct(ggplot2::guide_none(title = "my title", position = "top"))
})

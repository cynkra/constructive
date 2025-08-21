test_that("GuideCustom", {
  skip_if_not_installed("ggplot2")
  skip_if(with_versions(ggplot2 <= "3.5.2"))
  circle <- grid::circleGrob()
  expect_construct(
    ggplot2::guide_custom(circle, title = "My circle"),
    data = list(circle = circle)
  )
  expect_construct(
    ggplot2::guide_custom(
      circle,
      width = grid::unit(2, "cm"),
      height = grid::unit(2, "cm"),
      title = "My circle"
    ),
    data = list(circle = circle)
  )
  circle2 <- grid::circleGrob(r = grid::unit(1, "cm"))
  expect_construct(
    ggplot2::guide_custom(
      circle2,
      title = "My circle"
    ),
    data = list(circle2 = circle2)
  )
})

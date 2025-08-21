test_that("FacetGrid", {
  if (with_versions(R < "4.2.0")) skip_on_os("linux")
  mpg <- ggplot2::mpg
  p <- ggplot2::ggplot(mpg, ggplot2::aes(displ, cty)) + ggplot2::geom_point()
  p2 <- p + ggplot2::facet_grid(rows = ggplot2::vars(drv))

  p3 <- ggplot2::ggplot(mpg, ggplot2::aes(cty, hwy)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(year ~ drv, axes = "all", axis.labels = "all_x")

  p4 <- ggplot2::ggplot(mpg, ggplot2::aes(drv, model)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(manufacturer ~ ., scales = "free", space = "free") +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0))
  expect_snapshot({
    construct(p2, data = list(mpg = mpg))
    construct(p3, data = list(mpg = mpg))
    construct(p4, data = list(mpg = mpg))
  })
})

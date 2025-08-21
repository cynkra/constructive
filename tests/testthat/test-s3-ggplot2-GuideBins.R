test_that("GuideBins", {
  skip_if_not_installed("ggplot2")
  skip_if(with_versions(ggplot2 <= "3.5.2"))
  expect_construct(
    ggplot2::guide_bins(theme = ggplot2::theme(legend.axis.line = ggplot2::element_blank()))
  )
  my_arrow <- grid::arrow(length = grid::unit(1.5, "mm"))
  expect_construct(
    ggplot2::guide_bins(theme = ggplot2::theme(legend.axis.line = ggplot2::element_line(arrow = my_arrow))),
    data = list(my_arrow = my_arrow, ends = "both")
  )
})

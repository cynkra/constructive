test_that("GuideColourbar", {
  skip_if_not_installed("ggplot2")
  skip_if(with_versions(ggplot2 <= "3.5.2"))
  expect_construct(ggplot2::guide_colorbar())
  expect_construct(ggplot2::guide_colourbar(), ggplot2::guide_colorbar())
  expect_construct(
    ggplot2::guide_colorbar(theme = ggplot2::theme(
      legend.key.height = grid::unit(10, "lines"),
      legend.key.width  = grid::unit(0.5,"lines")
    ))
  )
  expect_construct(
    ggplot2::guide_colorbar(nbin = 100, draw.llim = FALSE, draw.ulim = FALSE)
  )
})

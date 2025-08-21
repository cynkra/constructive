test_that("GuideLegend", {
  skip_if_not_installed("ggplot2")
  skip_if(with_versions(ggplot2 <= "3.5.2"))
  expect_construct(ggplot2::guide_legend(
    title = "LEFT",
    theme = ggplot2::theme(legend.title.position = "left")
  ))
  expect_construct(ggplot2::guide_legend(theme = ggplot2::theme(
    legend.title = ggplot2::element_text(face = "italic", colour = "red", size = 15)
  )))
  expect_construct(ggplot2::guide_legend(override.aes = list(alpha = 1)))
  expect_construct(ggplot2::guide_legend(theme = ggplot2::theme(legend.byrow = TRUE), nrow = 8))
})

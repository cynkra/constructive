test_that("Guides", {
  skip_if_not_installed("ggplot2")
  skip_if(with_versions(ggplot2 <= "3.5.2"))
  expect_construct(
    ggplot2::guides(
      colour = ggplot2::guide_colorbar(order = 1),
      shape = ggplot2::guide_legend(order = 2),
      size = ggplot2::guide_legend(order = 3)
    )
  )
})

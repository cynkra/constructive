test_that("ggplot-all-versions", {
    mpg_99 <- ggplot2::mpg %>% dplyr::filter(year == 1999)
    base_99 <- ggplot2::ggplot(mpg_99, ggplot2::aes(displ, hwy)) + ggplot2::geom_point()
    p1 <- base_99 + ggplot2::scale_x_continuous(limits = c(1, 7))
    p2 <- p1 + ggplot2::scale_y_continuous(limits = c(10, 45)) + ggplot2::facet_wrap(~manufacturer)

    expect_faithful_ggplot_construction(base_99, opts_Layer("layer"))
    expect_faithful_ggplot_construction(base_99, opts_Layer("environment"))
    expect_faithful_ggplot_construction(p1, opts_Layer("environment"))
    expect_faithful_ggplot_construction(p2, opts_Layer("environment"))

    expect_snapshot({
      construct(p1, data = list(mpg_99 = mpg_99))
    })
})

# note: these tests are the same but the snapshot is different, says the object is not perfectly
# rebult with 4.1.3 and less
test_that("ggplot-after-R4.1.3", {
  skip_if(with_versions(R <= "4.1.3"))
  mpg_99 <- ggplot2::mpg %>% dplyr::filter(year == 1999)
  base_99 <- ggplot2::ggplot(mpg_99, ggplot2::aes(displ, hwy)) + ggplot2::geom_point()
  p1 <- base_99 + ggplot2::scale_x_continuous(limits = c(1, 7))
  p2 <- p1 + ggplot2::scale_y_continuous(limits = c(10, 45)) + ggplot2::facet_wrap(~manufacturer)

  expect_snapshot({
    construct(p2, data = list(mpg_99 = mpg_99))
  })
})

test_that("ggplot-pre-incl-R4.1.3", {
  skip_if(with_versions(R > "4.1.3"))
  mpg_99 <- ggplot2::mpg %>% dplyr::filter(year == 1999)
  base_99 <- ggplot2::ggplot(mpg_99, ggplot2::aes(displ, hwy)) + ggplot2::geom_point()
  p1 <- base_99 + ggplot2::scale_x_continuous(limits = c(1, 7))
  p2 <- p1 + ggplot2::scale_y_continuous(limits = c(10, 45)) + ggplot2::facet_wrap(~manufacturer)

  expect_snapshot({
    construct(p2, data = list(mpg_99 = mpg_99))
  })
})

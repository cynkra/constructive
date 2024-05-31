test_that("ggplot", {
    mpg_99 <- ggplot2::mpg %>% dplyr::filter(year == 1999)
    base_99 <- ggplot2::ggplot(mpg_99, ggplot2::aes(displ, hwy)) + ggplot2::geom_point()
    p1 <- base_99 + ggplot2::scale_x_continuous(limits = c(1, 7))
    p2 <- p1 + ggplot2::scale_y_continuous(limits = c(10, 45)) + ggplot2::facet_wrap(~manufacturer)

    expect_pipe_snapshot({
      construct(p1, data = list(mpg_99 = mpg_99))
      construct(p2, data = list(mpg_99 = mpg_99))
    })
    expect_faithful_ggplot_construction(base_99, opts_Layer("layer"))
    expect_faithful_ggplot_construction(base_99, opts_Layer("environment"))
    expect_faithful_ggplot_construction(p1, opts_Layer("environment"))
    expect_faithful_ggplot_construction(p2, opts_Layer("environment"))
})

test_that("ggplot", {
  expect_pipe_snapshot({
    mpg_99 <- ggplot2::mpg %>% dplyr::filter(year == 1999)
    base_99 <- ggplot2::ggplot(mpg_99, ggplot2::aes(displ, hwy)) + ggplot2::geom_point()

    construct(base_99, data = tibble::lst(mpg_99), opts_Layer("layer"), check = FALSE)
    construct(base_99, data = tibble::lst(mpg_99), opts_Layer("environment"), check = FALSE)

    p1 <- base_99 + ggplot2::scale_x_continuous(limits = c(1, 7))
    construct(p1, data = tibble::lst(mpg_99), check = FALSE)

    p2 <- p1 + ggplot2::scale_y_continuous(limits = c(10, 45)) + ggplot2::facet_wrap(~manufacturer)
    construct(p2, data = tibble::lst(mpg_99), check = FALSE)
  })
})

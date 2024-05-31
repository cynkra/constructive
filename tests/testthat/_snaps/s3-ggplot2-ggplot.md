# ggplot

    Code
      construct(p1, data = list(mpg_99 = mpg_99))
    Output
      mpg_99 |>
        ggplot2::ggplot(ggplot2::aes(displ, hwy)) +
        ggplot2::geom_point() +
        ggplot2::scale_x_continuous(limits = c(1, 7))
    Code
      construct(p2, data = list(mpg_99 = mpg_99))
    Output
      mpg_99 |>
        ggplot2::ggplot(ggplot2::aes(displ, hwy)) +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(facets = ggplot2::vars(manufacturer), dir = "h") +
        ggplot2::scale_x_continuous(limits = c(1, 7)) +
        ggplot2::scale_y_continuous(limits = c(10, 45))


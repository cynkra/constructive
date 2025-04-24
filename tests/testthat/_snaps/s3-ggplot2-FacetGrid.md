# FacetGrid

    Code
      construct(p2, data = list(mpg = mpg))
    Output
      mpg |>
        ggplot2::ggplot(ggplot2::aes(displ, cty)) +
        ggplot2::geom_point() +
        ggplot2::facet_grid(rows = ggplot2::vars(drv))
    Code
      construct(p3, data = list(mpg = mpg))
    Output
      mpg |>
        ggplot2::ggplot(ggplot2::aes(cty, hwy)) +
        ggplot2::geom_point() +
        ggplot2::facet_grid(rows = year ~ drv, axes = "all", axis.labels = "all_x")
    Code
      construct(p4, data = list(mpg = mpg))
    Output
      mpg |>
        ggplot2::ggplot(ggplot2::aes(drv, model)) +
        ggplot2::geom_point() +
        ggplot2::facet_grid(rows = manufacturer ~ ., scales = "free", space = "free") +
        ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0))


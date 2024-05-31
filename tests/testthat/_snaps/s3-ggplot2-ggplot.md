# ggplot-all-versions

    Code
      construct(p1, data = list(mpg_99 = mpg_99))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      mpg_99 |>
        ggplot2::ggplot(ggplot2::aes(displ, hwy)) +
        ggplot2::geom_point() +
        ggplot2::scale_x_continuous(limits = c(1, 7))

# ggplot-after-R4.1.3

    Code
      construct(p2, data = list(mpg_99 = mpg_99))
    Output
      mpg_99 |>
        ggplot2::ggplot(ggplot2::aes(displ, hwy)) +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(facets = ggplot2::vars(manufacturer)) +
        ggplot2::scale_x_continuous(limits = c(1, 7)) +
        ggplot2::scale_y_continuous(limits = c(10, 45))

# ggplot-pre-incl-R4.1.3

    Code
      construct(p2, data = list(mpg_99 = mpg_99))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      mpg_99 |>
        ggplot2::ggplot(ggplot2::aes(displ, hwy)) +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(facets = ggplot2::vars(manufacturer)) +
        ggplot2::scale_x_continuous(limits = c(1, 7)) +
        ggplot2::scale_y_continuous(limits = c(10, 45))


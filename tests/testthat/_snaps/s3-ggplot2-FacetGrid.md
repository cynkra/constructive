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
    Message
      ! The code built by {constructive} could not be evaluated.
      ! Due to error: No environment was found at the memory address '0x123456789'
      i It's likely that {constructive} was called in a different session to generate this code.
      i The environment might also have been garbage collected.
      i See `?opts_environment` for various alternatives to construct environment with persistent definitions.
    Output
      mpg |>
        ggplot2::ggplot(ggplot2::aes(cty, hwy)) +
        ggplot2::geom_point() +
        ggplot2::facet_grid(
          rows = (year ~ drv) |>
            structure(
              .Environment = constructive::.env(
                "0x123456789",
                parents = c("0x123456789", "0x123456789", "namespace:constructive")
              )
            ),
          axes = "all",
          axis.labels = "all_x"
        )
    Code
      construct(p4, data = list(mpg = mpg), check = FALSE)
    Output
      mpg |>
        ggplot2::ggplot(ggplot2::aes(drv, model)) +
        ggplot2::geom_point() +
        ggplot2::facet_grid(
          rows = (manufacturer ~ .) |>
            structure(
              .Environment = constructive::.env(
                "0x123456789",
                parents = c("0x123456789", "0x123456789", "namespace:constructive")
              )
            ),
          scales = "free",
          space = "free"
        ) +
        ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0))


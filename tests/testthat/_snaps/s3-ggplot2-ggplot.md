# ggplot

    Code
      mpg_99 <- ggplot2::mpg %>% dplyr::filter(year == 1999)
      base_99 <- ggplot2::ggplot(mpg_99, ggplot2::aes(displ, hwy)) + ggplot2::geom_point()
      construct(base_99, data = tibble::lst(mpg_99), opts_Layer("layer"), check = FALSE)
    Output
      mpg_99 |>
        ggplot2::ggplot(ggplot2::aes(displ, hwy)) +
        ggplot2::layer(
          stat = "identity",
          position = "identity",
          params = list(na.rm = FALSE),
          geom = "point"
        )
    Code
      construct(base_99, data = tibble::lst(mpg_99), opts_Layer("environment"),
      check = FALSE)
    Output
      mpg_99 |>
        ggplot2::ggplot(ggplot2::aes(displ, hwy)) +
        constructive::.env(
          "0x000000000",
          parents = "empty",
          class = c("LayerInstance", "Layer", "ggproto", "gg")
        ) |>
          structure(class = c("LayerInstance", "Layer", "ggproto", "gg"))
    Code
      p1 <- base_99 + ggplot2::scale_x_continuous(limits = c(1, 7))
      construct(p1, data = tibble::lst(mpg_99), check = FALSE)
    Output
      mpg_99 |>
        ggplot2::ggplot(ggplot2::aes(displ, hwy)) +
        ggplot2::geom_point() +
        ggplot2::xlim(1, 7)
    Code
      p2 <- p1 + ggplot2::scale_y_continuous(limits = c(10, 45)) + ggplot2::facet_wrap(
        ~manufacturer)
      construct(p2, data = tibble::lst(mpg_99), check = FALSE)
    Output
      mpg_99 |>
        ggplot2::ggplot(ggplot2::aes(displ, hwy)) +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(manufacturer),
          labeller = (function(labels, multi_line = TRUE) {
            labels <- lapply(labels, as.character)
            if (multi_line) {
              labels
            } else {
              collapse_labels_lines(labels)
            }
          }) |>
            (`environment<-`)(asNamespace("ggplot2")) |>
            structure(class = c("function", "labeller"))
        ) +
        ggplot2::xlim(1, 7) +
        ggplot2::ylim(10, 45)


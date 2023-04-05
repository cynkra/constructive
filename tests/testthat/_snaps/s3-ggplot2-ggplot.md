# ggplot

    Code
      library(dplyr, warn = FALSE)
      library(ggplot2, warn = FALSE)
      mpg_99 <- mpg %>% filter(year == 1999)
      base_99 <- ggplot(mpg_99, aes(displ, hwy)) + geom_point()
      p <- base_99 + scale_x_continuous(limits = c(1, 7)) + scale_y_continuous(
        limits = c(10, 45))
      construct(p, data = lst(mpg_99), check = FALSE)
    Output
      mpg_99 |>
        ggplot2::ggplot(ggplot2::aes(displ, hwy)) +
        ggplot2::geom_point() +
        ggplot2::xlim(1, 7) +
        ggplot2::ylim(10, 45)


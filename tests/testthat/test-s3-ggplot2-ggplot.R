test_that("ggplot", {
  expect_snapshot({
    library(dplyr, warn = FALSE)
    library(ggplot2, warn = FALSE)
    mpg_99 <- mpg %>% filter(year == 1999)
    base_99 <- ggplot(mpg_99, aes(displ, hwy)) + geom_point()

    construct(base_99, data = lst(mpg_99), opts_Layer("layer"), check = FALSE)
    construct(base_99, data = lst(mpg_99), opts_Layer("environment"), check = FALSE)

    p1 <- base_99  + scale_x_continuous(limits = c(1, 7))
    construct(p1, data = lst(mpg_99), check = FALSE)

    p2 <- p1 + scale_y_continuous(limits = c(10, 45)) + facet_wrap(~manufacturer)
    construct(p2, data = lst(mpg_99), check = FALSE)
  })
})

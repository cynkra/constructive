test_that("ggplot", {
  expect_snapshot({
    library(dplyr)
    library(ggplot2)
    mpg_99 <- mpg %>% filter(year == 1999)
    base_99 <- ggplot(mpg_99, aes(displ, hwy)) + geom_point()
    p <-
      base_99 +
      scale_x_continuous(limits = c(1, 7)) +
      scale_y_continuous(limits = c(10, 45))
    construct(p, data = lst(mpg_99))
  })
})

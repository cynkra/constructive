test_that("theme", {
  expect_snapshot({
    construct(ggplot2::theme_bw())
    construct(ggplot2::theme_bw(base_size = 11, base_line_size = 0.5))
    construct(ggplot2::theme_bw(base_size = 22, base_line_size = 0.5))
    construct(ggplot2::theme_bw(base_size = 11, base_line_size = 1))
    construct(ggplot2::theme_bw(base_size = 22, base_line_size = 1))
    construct(ggplot2::theme())
    construct(ggplot2::theme(axis.title = "foo"))
  })
})

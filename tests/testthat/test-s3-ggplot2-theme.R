test_that("theme", {
  skip_if(with_versions(R < "4.3"))
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

test_that("theme with `constructor = \"theme\"`", {
  skip_if_not_installed("ggplot2")
  # the default constructor guesses complete themes
  code <- construct(ggplot2::theme_bw())
  expect_identical(unclass(code$code), "ggplot2::theme_bw()")
  # the "theme" constructor always uses `theme()`, the full output depends on
  # the ggplot2 version so it's not snapshotted
  code <- construct(ggplot2::theme_bw(), opts_theme("theme"), opts_ggplot2_theme("theme"))
  expect_match(code$code[[1]], "^ggplot2::theme\\(")
  expect_null(construct_issues())
})

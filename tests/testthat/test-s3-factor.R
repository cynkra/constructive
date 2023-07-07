test_that("factor", {
  expect_pipe_snapshot({
    # simple factor with implict levels
    construct(factor(month.abb))
    construct(factor(month.abb), opts_factor("next"))
    construct(factor(month.abb), opts_factor("atomic"))
    construct(factor(month.abb, month.abb))
    construct(factor(month.abb), opts_factor("as_factor"))
    construct(factor(month.abb, month.abb), opts_factor("as_factor"))
    construct(factor(month.abb), opts_factor("new_factor"))
    construct(factor(month.abb, month.abb), opts_factor("new_factor"))
    construct(factor(month.abb, levels = c(month.abb, NA), exclude = NULL))
    construct(factor(c(a="foo")))
  })
})

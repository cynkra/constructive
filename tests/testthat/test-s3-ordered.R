test_that("ordered", {
  expect_snapshot({
    # ordered
    construct(factor(month.abb, ordered = TRUE))
    construct(factor(month.abb, ordered = TRUE))
    construct(factor(month.abb, ordered = TRUE), opts_ordered("factor"))
    construct(factor(month.abb, ordered = TRUE), opts_ordered("new_ordered"))
    construct(factor(month.abb, ordered = TRUE), opts_ordered("next"))
    construct(factor(month.abb, ordered = TRUE), opts_ordered("atomic"))
    construct(factor(month.abb, month.abb, ordered = TRUE))
    construct(factor(month.abb, month.abb, ordered = TRUE), opts_ordered("factor"))
    construct(factor(month.abb, month.abb, ordered = TRUE), opts_ordered("new_ordered"))
    construct(ordered(c(a="foo")))
  })
})

test_that("ordered", {
  expect_snapshot({
    # ordered
    construct(factor(month.abb, rev(month.abb), ordered = TRUE))
  })
})

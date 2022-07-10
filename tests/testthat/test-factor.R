test_that("factor", {
  expect_snapshot({
    # simple factor with implict levels
    construct(factor(month.abb))
    # explicit levels
    construct(factor(month.abb, rev(month.abb)))
  })
})

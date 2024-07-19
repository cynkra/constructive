test_that("yearqtr", {
  expect_snapshot({
    x <- zoo::as.yearqtr("2001 Q2")
    construct(x)
    construct(x, opts_yearqtr("yearqtr"))
  })
})

test_that("yearmon", {
  expect_snapshot({
    x <- zoo::as.yearmon("2007-12")
    construct(x)
    construct(x, opts_yearmon("yearmon"))
  })
})

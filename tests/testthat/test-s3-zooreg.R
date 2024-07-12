test_that("zooreg", {
  expect_snapshot({
    construct(zoo::zooreg(1:10, frequency = 4, start = c(1959, 2)))
    construct(zoo::zoo(1:10, zoo::yearqtr(seq(1959.25, 1961.5, by = 0.25)), frequency = 4))
    construct(zoo::zooreg(1:5, start = as.Date("2000-01-01")))
    construct(zoo::zooreg(1:5, end = zoo::yearmon(2000)))
  })
})

test_that("pipe works for one liners", {
  expect_snapshot({
    x <- 1
    attr(x, "foo") <- 2
    construct(x, one_liner = TRUE)
  })
})

test_that("data", {
  expect_snapshot({
    construct(cars, data = "datasets")
    construct(mean, data = "base")
    construct(mean, data = asNamespace("base"))
    construct(list(mean, cars), data = list(asNamespace("base"), "datasets"))
  })
})

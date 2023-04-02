test_that("externalptr", {
  expect_null({
    dt <- data.table::data.table(a = 1)
    class(dt) <- "data.frame"
    construct_issues(construct(dt))
  })
})

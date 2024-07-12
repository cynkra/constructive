test_that("zoo", {
  expect_snapshot({
    construct(zoo::zoo(
      c(-0.06, -0.16, -1.47, -0.48, 0.42),
      as.Date("2003-02-01") + c(1, 3, 7, 9, 14) - 1
    ))
    construct(zoo::zoo(c(-0.06, -0.16, -1.47, -0.48, 0.42), c(1, 3, 7, 9, 14)))
    construct(zoo::zoo(
      c(-0.06, -0.16, -1.47, -0.48, 0.42),
      ISOdatetime(2003, 02, c(1, 3, 7, 9, 14), 0, 0, 0)
    ))
  })
})

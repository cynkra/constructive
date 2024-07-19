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
    mat <- matrix(
      1:8,
      nrow = 2L,
      ncol = 4L,
      dimnames = list(
        c("2007-01-02", "2007-01-03"),
        c("Open", "High", "Low", "Close")
      )
    )
    construct(zoo::as.zoo(mat))
  })
})

test_that("ftable", {
  expect_snapshot({
    construct(ftable(table(a = c(1, 2, 2), b = c("x", "y", "y"))))
    construct(ftable(as.table(matrix(1:4, 2))))
    construct(ftable(Titanic, row.vars = 1:2))
    construct(ftable(Titanic, row.vars = c(4, 1), col.vars = 2))
    construct(ftable(table(a = c(1, 2, 2), b = c("x", "y", "y"))), opts_ftable("next"))
  })
})

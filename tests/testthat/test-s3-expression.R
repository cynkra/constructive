test_that("expression vectors", {
  expect_pipe_snapshot({
    x <- expression(a = 1, x + y)
    construct(x)
    x[[2]] <- structure(quote(x + y), foo = 1)
    construct(x)
    x[[2]] <- expression(x + y)
    construct(x)
    names(x)[[1]] <- NA
    construct(x)
  })
})

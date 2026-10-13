test_that("indMatrix", {
  skip_if_not_installed("Matrix")
  expect_snapshot({
    # pMatrix
    construct(methods::as(c(2L, 3L, 1L), "pMatrix"))
    construct(methods::as(c(2L, 3L, 1L), "pMatrix"), opts_indMatrix("next"))
    construct(Matrix::t(methods::as(c(2L, 3L, 1L), "pMatrix")))
    # indMatrix
    construct(methods::as(c(1L, 1L, 2L), "indMatrix"))
    construct(methods::as(list(c(1L, 1L, 2L), 4L), "indMatrix"))
    construct(Matrix::t(methods::as(c(1L, 1L, 2L), "indMatrix")))
    # dimnames can't be set by `as()`, we fall back
    x <- methods::as(c(2L, 3L, 1L), "pMatrix")
    x@Dimnames <- list(c("a", "b", "c"), NULL)
    construct(x)
  })
})

# The shape of an index matrix changed twice, so the code we generate differs
# between three generations of Matrix (see its NEWS):
#   < 1.6-0   `perm`, `Dim`, `Dimnames`, `factors`, and `t()` rebuilds the object
#   1.6-x     gains `margin`, still inherits `factors`
#   >= 1.7-0  no longer inherits `factors`
# All three reproduce the input faithfully, so we pin all three.

test_that("indMatrix, Matrix >= 1.7-0", {
  skip_if_not_installed("Matrix")
  skip_if(with_versions(Matrix < "1.7.0"))
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

test_that("indMatrix, Matrix 1.6-0 to 1.6-x", {
  skip_if_not_installed("Matrix")
  skip_if(with_versions(Matrix < "1.6.0" || Matrix >= "1.7.0"))
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

test_that("indMatrix, Matrix < 1.6-0", {
  skip_if_not_installed("Matrix")
  skip_if(with_versions(Matrix >= "1.6.0"))
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

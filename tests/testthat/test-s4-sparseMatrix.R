test_that("sparseMatrix", {
  skip_if_not_installed("Matrix")
  expect_snapshot({
    # dgCMatrix
    construct(Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), x = c(1.5, 2), dims = c(3, 4)))
    construct(
      Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), x = c(1.5, 2), dims = c(3, 4)),
      opts_sparseMatrix("next")
    )
    # dgTMatrix, dgRMatrix
    construct(Matrix::sparseMatrix(i = c(3, 1), j = c(1, 2), x = c(1, 2), dims = c(3, 3), repr = "T"))
    construct(Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), x = c(1.5, 2), dims = c(3, 4), repr = "R"))
    # lgCMatrix, ngCMatrix
    construct(Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), x = c(TRUE, NA), dims = c(3, 4)))
    construct(Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), dims = c(3, 4)))
    # dsCMatrix, dtCMatrix
    construct(Matrix::sparseMatrix(i = c(1, 2), j = c(2, 3), x = c(1.5, 2), dims = c(3, 3), symmetric = TRUE))
    construct(Matrix::sparseMatrix(i = c(2, 3), j = c(1, 2), x = c(1.5, 2), dims = c(3, 3), triangular = TRUE))
    # dimnames, empty
    construct(Matrix::sparseMatrix(
      i = 1, j = 2, x = 3, dims = c(2, 2), dimnames = list(c("a", "b"), c("A", "B"))
    ))
    construct(Matrix::sparseMatrix(i = integer(0), j = integer(0), x = numeric(0), dims = c(0, 0)))
    # unit triangular matrices can't be built by `sparseMatrix()`, we fall back
    construct(Matrix::diagN2U(Matrix::sparseMatrix(i = 1:2, j = 1:2, x = 1, triangular = TRUE)))
    # attributes to repair
    x <- Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), x = c(1.5, 2), dims = c(3, 4))
    attr(x, "foo") <- "bar"
    construct(x)
  })
})

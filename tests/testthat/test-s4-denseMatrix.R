test_that("denseMatrix", {
  skip_if_not_installed("Matrix")
  expect_snapshot({
    # dgeMatrix, dsyMatrix, dtrMatrix, lgeMatrix
    construct(Matrix::Matrix(matrix(c(1.5, 2, 3, 4), 2), sparse = FALSE))
    construct(Matrix::Matrix(matrix(c(1.5, 2, 3, 4), 2), sparse = FALSE), opts_denseMatrix("next"))
    construct(Matrix::Matrix(matrix(c(1, 2, 2, 4), 2), sparse = FALSE))
    construct(Matrix::Matrix(matrix(c(1, 0, 2, 4), 2), sparse = FALSE))
    construct(Matrix::Matrix(matrix(c(TRUE, FALSE, NA, TRUE), 2), sparse = FALSE))
    # dimnames, empty
    construct(Matrix::Matrix(
      matrix(1:6 + 0.5, 2, dimnames = list(c("a", "b"), c("A", "B", "C"))),
      sparse = FALSE
    ))
    construct(Matrix::Matrix(matrix(numeric(0), 0, 2), sparse = FALSE))
    # a general matrix with symmetric content can't be built by `Matrix()`, we fall back
    construct(Matrix::unpack(methods::as(
      Matrix::Matrix(matrix(c(1, 2, 2, 4), 2), sparse = FALSE),
      "generalMatrix"
    )))
  })
})

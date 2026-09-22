test_that("diagonalMatrix", {
  skip_if_not_installed("Matrix")
  expect_snapshot({
    construct(Matrix::Diagonal(3))
    construct(Matrix::Diagonal(3, TRUE))
    construct(Matrix::Diagonal(2, c(1.5, 2)))
    construct(Matrix::Diagonal(2, c(1.5, 2)), opts_diagonalMatrix("next"))
    construct(Matrix::Diagonal(2, c(TRUE, NA)))
    construct(Matrix::Diagonal(2, c(a = 1.5, b = 2), names = TRUE))
    construct(Matrix::Diagonal(0))
    # different row and column names can't be built by `Diagonal()`, we fall back
    x <- Matrix::Diagonal(2, c(1.5, 2))
    x@Dimnames <- list(c("a", "b"), NULL)
    construct(x)
  })
})

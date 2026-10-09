# diagonalMatrix

    Code
      construct(Matrix::Diagonal(3))
    Output
      Matrix::Diagonal(3)
    Code
      construct(Matrix::Diagonal(3, TRUE))
    Output
      Matrix::Diagonal(3, x = TRUE)
    Code
      construct(Matrix::Diagonal(2, c(1.5, 2)))
    Output
      Matrix::Diagonal(2, x = c(1.5, 2))
    Code
      construct(Matrix::Diagonal(2, c(1.5, 2)), opts_diagonalMatrix("next"))
    Output
      new(
        "ddiMatrix" |>
          structure(package = "Matrix"),
        diag = "N",
        Dim = c(2L, 2L),
        Dimnames = list(NULL, NULL),
        x = c(1.5, 2)
      )
    Code
      construct(Matrix::Diagonal(2, c(TRUE, NA)))
    Output
      Matrix::Diagonal(2, x = c(TRUE, NA))
    Code
      construct(Matrix::Diagonal(2, c(a = 1.5, b = 2), names = TRUE))
    Output
      Matrix::Diagonal(2, x = c(1.5, 2), names = c("a", "b"))
    Code
      construct(Matrix::Diagonal(0))
    Output
      Matrix::Diagonal(0)
    Code
      x <- Matrix::Diagonal(2, c(1.5, 2))
      x@Dimnames <- list(c("a", "b"), NULL)
      construct(x)
    Output
      new(
        "ddiMatrix" |>
          structure(package = "Matrix"),
        diag = "N",
        Dim = c(2L, 2L),
        Dimnames = list(c("a", "b"), NULL),
        x = c(1.5, 2)
      )


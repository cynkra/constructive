# sparseVector

    Code
      construct(Matrix::sparseVector(c(1.5, NA), c(2, 5), 10))
    Output
      Matrix::sparseVector(x = c(1.5, NA), i = c(2, 5), length = 10)
    Code
      construct(Matrix::sparseVector(c(1.5, NA), c(2, 5), 10), opts_sparseVector(
        "next"))
    Output
      new(
        "dsparseVector" |>
          structure(package = "Matrix"),
        x = c(1.5, NA),
        length = 10,
        i = c(2, 5)
      )
    Code
      construct(Matrix::sparseVector(c(1L, 3L), c(2L, 5L), 10L))
    Output
      Matrix::sparseVector(x = c(1L, 3L), i = c(2L, 5L), length = 10L)
    Code
      construct(Matrix::sparseVector(c(TRUE, NA), c(2, 5), 10))
    Output
      Matrix::sparseVector(x = c(TRUE, NA), i = c(2, 5), length = 10)
    Code
      construct(Matrix::sparseVector(i = c(2, 5), length = 10))
    Output
      Matrix::sparseVector(i = c(2, 5), length = 10)
    Code
      construct(Matrix::sparseVector(0+1i, 2, 3))
    Output
      Matrix::sparseVector(x = 1i, i = 2, length = 3)
    Code
      construct(Matrix::sparseVector(numeric(0), integer(0), 0))
    Output
      Matrix::sparseVector(x = numeric(0), i = integer(0), length = 0)


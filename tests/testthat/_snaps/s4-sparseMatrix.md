# sparseMatrix

    Code
      construct(Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), x = c(1.5, 2), dims = c(
        3, 4)))
    Output
      Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), x = c(1.5, 2), dims = c(3, 4))
    Code
      construct(Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), x = c(1.5, 2), dims = c(
        3, 4)), opts_sparseMatrix("next"))
    Output
      new(
        "dgCMatrix" |>
          structure(package = "Matrix"),
        i = c(0L, 2L),
        p = c(0L, 0L, 1L, 2L, 2L),
        Dim = 3:4,
        Dimnames = list(NULL, NULL),
        x = c(1.5, 2),
        factors = list()
      )
    Code
      construct(Matrix::sparseMatrix(i = c(3, 1), j = c(1, 2), x = c(1, 2), dims = c(
        3, 3), repr = "T"))
    Output
      Matrix::sparseMatrix(i = c(3, 1), j = c(1, 2), x = c(1, 2), dims = c(3, 3), repr = "T")
    Code
      construct(Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), x = c(1.5, 2), dims = c(
        3, 4), repr = "R"))
    Output
      Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), x = c(1.5, 2), dims = c(3, 4), repr = "R")
    Code
      construct(Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), x = c(TRUE, NA), dims = c(
        3, 4)))
    Output
      Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), x = c(TRUE, NA), dims = c(3, 4))
    Code
      construct(Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), dims = c(3, 4)))
    Output
      Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), dims = c(3, 4))
    Code
      construct(Matrix::sparseMatrix(i = c(1, 2), j = c(2, 3), x = c(1.5, 2), dims = c(
        3, 3), symmetric = TRUE))
    Output
      Matrix::sparseMatrix(i = c(1, 2), j = c(2, 3), x = c(1.5, 2), dims = c(3, 3), symmetric = TRUE)
    Code
      construct(Matrix::sparseMatrix(i = c(2, 3), j = c(1, 2), x = c(1.5, 2), dims = c(
        3, 3), triangular = TRUE))
    Output
      Matrix::sparseMatrix(i = c(2, 3), j = c(1, 2), x = c(1.5, 2), dims = c(3, 3), triangular = TRUE)
    Code
      construct(Matrix::sparseMatrix(i = 1, j = 2, x = 3, dims = c(2, 2), dimnames = list(
        c("a", "b"), c("A", "B"))))
    Output
      Matrix::sparseMatrix(i = 1, j = 2, x = 3, dims = c(2, 2), dimnames = list(c("a", "b"), c("A", "B")))
    Code
      construct(Matrix::sparseMatrix(i = integer(0), j = integer(0), x = numeric(0),
      dims = c(0, 0)))
    Output
      Matrix::sparseMatrix(i = numeric(0), j = numeric(0), x = numeric(0), dims = c(0, 0))
    Code
      construct(Matrix::diagN2U(Matrix::sparseMatrix(i = 1:2, j = 1:2, x = 1,
      triangular = TRUE)))
    Output
      new(
        "dtCMatrix" |>
          structure(package = "Matrix"),
        i = integer(0),
        p = integer(3),
        Dim = c(2L, 2L),
        Dimnames = list(NULL, NULL),
        x = numeric(0),
        uplo = "L",
        diag = "U"
      )
    Code
      x <- Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), x = c(1.5, 2), dims = c(3,
        4))
      attr(x, "foo") <- "bar"
      construct(x)
    Output
      Matrix::sparseMatrix(i = c(1, 3), j = c(2, 3), x = c(1.5, 2), dims = c(3, 4)) |>
        structure(foo = "bar")


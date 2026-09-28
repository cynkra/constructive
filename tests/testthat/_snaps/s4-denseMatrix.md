# denseMatrix

    Code
      construct(Matrix::Matrix(matrix(c(1.5, 2, 3, 4), 2), sparse = FALSE))
    Output
      Matrix::Matrix(matrix(c(1.5, 2, 3, 4), nrow = 2L, ncol = 2L), sparse = FALSE)
    Code
      construct(Matrix::Matrix(matrix(c(1.5, 2, 3, 4), 2), sparse = FALSE),
      opts_denseMatrix("next"))
    Output
      new(
        "dgeMatrix" |>
          structure(package = "Matrix"),
        Dim = c(2L, 2L),
        Dimnames = list(NULL, NULL),
        x = c(1.5, 2, 3, 4),
        factors = list()
      )
    Code
      construct(Matrix::Matrix(matrix(c(1, 2, 2, 4), 2), sparse = FALSE))
    Output
      Matrix::Matrix(matrix(c(1, 2, 2, 4), nrow = 2L, ncol = 2L), sparse = FALSE)
    Code
      construct(Matrix::Matrix(matrix(c(1, 0, 2, 4), 2), sparse = FALSE))
    Output
      Matrix::Matrix(matrix(c(1, 0, 2, 4), nrow = 2L, ncol = 2L), sparse = FALSE)
    Code
      construct(Matrix::Matrix(matrix(c(TRUE, FALSE, NA, TRUE), 2), sparse = FALSE))
    Output
      Matrix::Matrix(matrix(c(TRUE, FALSE, NA, TRUE), nrow = 2L, ncol = 2L), sparse = FALSE)
    Code
      construct(Matrix::Matrix(matrix(1:6 + 0.5, 2, dimnames = list(c("a", "b"), c(
        "A", "B", "C"))), sparse = FALSE))
    Output
      Matrix::Matrix(
        matrix(
          seq(1.5, 6.5, by = 1),
          nrow = 2L,
          ncol = 3L,
          dimnames = list(c("a", "b"), c("A", "B", "C"))
        ),
        sparse = FALSE
      )
    Code
      construct(Matrix::Matrix(matrix(numeric(0), 0, 2), sparse = FALSE))
    Output
      Matrix::Matrix(matrix(numeric(0), nrow = 0L, ncol = 2L), sparse = FALSE)
    Code
      construct(Matrix::unpack(methods::as(Matrix::Matrix(matrix(c(1, 2, 2, 4), 2),
      sparse = FALSE), "generalMatrix")))
    Output
      new(
        "dgeMatrix" |>
          structure(package = "Matrix"),
        Dim = c(2L, 2L),
        Dimnames = list(NULL, NULL),
        x = c(1, 2, 2, 4),
        factors = list()
      )


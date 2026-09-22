# indMatrix

    Code
      construct(methods::as(c(2L, 3L, 1L), "pMatrix"))
    Output
      as(c(2L, 3L, 1L), "pMatrix")
    Code
      construct(methods::as(c(2L, 3L, 1L), "pMatrix"), opts_indMatrix("next"))
    Output
      new(
        "pMatrix" |>
          structure(package = "Matrix"),
        perm = c(2L, 3L, 1L),
        margin = 1L,
        Dim = c(3L, 3L),
        Dimnames = list(NULL, NULL)
      )
    Code
      construct(Matrix::t(methods::as(c(2L, 3L, 1L), "pMatrix")))
    Output
      Matrix::t(as(c(2L, 3L, 1L), "pMatrix"))
    Code
      construct(methods::as(c(1L, 1L, 2L), "indMatrix"))
    Output
      as(c(1L, 1L, 2L), "indMatrix")
    Code
      construct(methods::as(list(c(1L, 1L, 2L), 4L), "indMatrix"))
    Output
      as(list(c(1L, 1L, 2L), 4L), "indMatrix")
    Code
      construct(Matrix::t(methods::as(c(1L, 1L, 2L), "indMatrix")))
    Output
      Matrix::t(as(c(1L, 1L, 2L), "indMatrix"))
    Code
      x <- methods::as(c(2L, 3L, 1L), "pMatrix")
      x@Dimnames <- list(c("a", "b", "c"), NULL)
      construct(x)
    Output
      new(
        "pMatrix" |>
          structure(package = "Matrix"),
        perm = c(2L, 3L, 1L),
        margin = 1L,
        Dim = c(3L, 3L),
        Dimnames = list(c("a", "b", "c"), NULL)
      )


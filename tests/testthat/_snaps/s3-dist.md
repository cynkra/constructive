# dist

    Code
      construct(as.dist(matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), 3)))
    Output
      as.dist(matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3L, ncol = 3L)) |>
        structure(call = quote(as.dist.default(m = matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), 3))))
    Code
      construct(dist(matrix(c(1, 2, 4, 3, 5, 6), 3)))
    Output
      as.dist(
        matrix(
          c(
            0, 2.23606797749979, 4.242640687119285, 2.23606797749979, 0, 2.23606797749979,
            4.242640687119285, 2.23606797749979, 0
          ),
          nrow = 3L,
          ncol = 3L
        )
      ) |>
        structure(method = "euclidean", call = quote(dist(x = matrix(c(1, 2, 4, 3, 5, 6), 3))))
    Code
      m <- matrix(c(1, 2, NA, 4, 5, 6), 3, dimnames = list(c("a", "b", "c"), NULL))
      construct(dist(m, method = "manhattan", diag = TRUE, upper = TRUE))
    Output
      as.dist(
        matrix(
          c(0, 2, 4, 2, 0, 2, 4, 2, 0),
          nrow = 3L,
          ncol = 3L,
          dimnames = list(c("a", "b", "c"), c("a", "b", "c"))
        ),
        diag = TRUE,
        upper = TRUE
      ) |>
        structure(
          method = "manhattan",
          call = quote(dist(x = m, method = "manhattan", diag = TRUE, upper = TRUE))
        )
    Code
      construct(dist(matrix(1:2, 1)))
    Output
      as.dist(matrix(0, nrow = 1L, ncol = 1L)) |>
        structure(method = "euclidean", call = quote(dist(x = matrix(1:2, 1))))
    Code
      construct(structure(as.dist(matrix(c(0, 1, 1, 0), 2)), call = NULL))
    Output
      as.dist(matrix(c(0, 1, 1, 0), nrow = 2L, ncol = 2L)) |>
        structure(call = NULL)
    Code
      construct(dist(matrix(c(1, 2, 4, 3, 5, 6), 3)), opts_dist("next"))
    Output
      c(2.23606797749979, 4.242640687119285, 2.23606797749979) |>
        structure(
          Size = 3L,
          Diag = FALSE,
          Upper = FALSE,
          method = "euclidean",
          call = quote(dist(x = matrix(c(1, 2, 4, 3, 5, 6), 3))),
          class = "dist"
        )


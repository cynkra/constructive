# mts

    Code
      construct(ts(matrix(1:9, 3, 3), start = c(1961, 1), frequency = 12))
    Output
      ts(
        matrix(
          1:9,
          nrow = 3L,
          ncol = 3L,
          dimnames = list(NULL, c("Series 1", "Series 2", "Series 3"))
        ),
        frequency = 12,
        start = 1961
      )
    Code
      construct(ts(matrix(1:9, 3, 3), start = c(1961, 1), frequency = 12), opts_mts(
        "next"))
    Output
      ts(
        matrix(
          1:9,
          nrow = 3L,
          ncol = 3L,
          dimnames = list(NULL, c("Series 1", "Series 2", "Series 3"))
        ) |>
          structure(class = c("mts", "matrix")),
        frequency = 12,
        start = 1961
      ) |>
        structure(
          dim = c(3L, 3L),
          dimnames = list(NULL, c("Series 1", "Series 2", "Series 3")),
          class = c("mts", "ts", "matrix")
        )
    Code
      construct(ts(matrix(1:9, 3, 3), start = c(1961, 1), frequency = 12), opts_mts(
        "atomic"))
    Output
      1:9 |>
        structure(
          dim = c(3L, 3L),
          dimnames = list(NULL, c("Series 1", "Series 2", "Series 3")),
          tsp = c(1961, 1961.1666666666667, 12),
          class = c("mts", "ts", "matrix")
        )


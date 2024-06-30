# `construct_dput()`, `construct_base()`, classes arg

    Code
      ts_ <- ts(1:10, frequency = 4, start = c(1959, 2))
      construct_dput(ts_)
    Output
      1:10 |>
        structure(tsp = c(1959.25, 1961.5, 4), class = "ts")
    Code
      construct_base(ts_)
    Output
      ts(1:10, frequency = 4, start = 1959.25)
    Code
      construct(ts_, classes = "{base}")
    Output
      1:10 |>
        structure(tsp = c(1959.25, 1961.5, 4), class = "ts")
    Code
      iris2 <- head(iris, 2)
      construct_dput(iris2)
    Output
      list(
        Sepal.Length = c(5.1, 4.9),
        Sepal.Width = c(3.5, 3),
        Petal.Length = c(1.4, 1.4),
        Petal.Width = c(0.2, 0.2),
        Species = c(1L, 1L) |>
          structure(levels = c("setosa", "versicolor", "virginica"), class = "factor")
      ) |>
        structure(row.names = c(NA, -2L), class = "data.frame")
    Code
      construct_base(iris2)
    Output
      data.frame(
        Sepal.Length = c(5.1, 4.9),
        Sepal.Width = c(3.5, 3),
        Petal.Length = c(1.4, 1.4),
        Petal.Width = c(0.2, 0.2),
        Species = factor(c("setosa", "setosa"), levels = c("setosa", "versicolor", "virginica"))
      )
    Code
      construct(iris2, classes = "{base}")
    Output
      data.frame(
        Sepal.Length = c(5.1, 4.9),
        Sepal.Width = c(3.5, 3),
        Petal.Length = c(1.4, 1.4),
        Petal.Width = c(0.2, 0.2),
        Species = factor(c("setosa", "setosa"), levels = c("setosa", "versicolor", "virginica"))
      )
    Code
      construct(iris2, classes = "-{base}")
    Output
      list(
        Sepal.Length = c(5.1, 4.9),
        Sepal.Width = c(3.5, 3),
        Petal.Length = c(1.4, 1.4),
        Petal.Width = c(0.2, 0.2),
        Species = c(1L, 1L) |>
          structure(levels = c("setosa", "versicolor", "virginica"), class = "factor")
      ) |>
        structure(row.names = c(NA, -2L), class = "data.frame")
    Code
      construct(iris2, classes = "factor")
    Output
      list(
        Sepal.Length = c(5.1, 4.9),
        Sepal.Width = c(3.5, 3),
        Petal.Length = c(1.4, 1.4),
        Petal.Width = c(0.2, 0.2),
        Species = factor(c("setosa", "setosa"), levels = c("setosa", "versicolor", "virginica"))
      ) |>
        structure(row.names = c(NA, -2L), class = "data.frame")
    Code
      construct(iris2, classes = "-factor")
    Output
      data.frame(
        Sepal.Length = c(5.1, 4.9),
        Sepal.Width = c(3.5, 3),
        Petal.Length = c(1.4, 1.4),
        Petal.Width = c(0.2, 0.2),
        Species = c(1L, 1L) |>
          structure(levels = c("setosa", "versicolor", "virginica"), class = "factor")
      )
    Code
      construct_dput(dplyr::band_members)
    Output
      list(name = c("Mick", "John", "Paul"), band = c("Stones", "Beatles", "Beatles")) |>
        structure(class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -3L))
    Code
      construct_base(dplyr::band_members)
    Output
      data.frame(name = c("Mick", "John", "Paul"), band = c("Stones", "Beatles", "Beatles")) |>
        structure(class = c("tbl_df", "tbl", "data.frame"))


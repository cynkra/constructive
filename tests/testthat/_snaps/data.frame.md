# data.frame

    Code
      construct(head(cars, 2))
    Output
      data.frame(speed = c(4, 4), dist = c(2, 10))
    Code
      construct(head(mtcars, 2))
    Output
      data.frame(
        mpg = c(21, 21),
        cyl = c(6, 6),
        disp = c(160, 160),
        hp = c(110, 110),
        drat = c(3.9, 3.9),
        wt = c(2.62, 2.875),
        qsec = c(16.46, 17.02),
        vs = c(0, 0),
        am = c(1, 1),
        gear = c(4, 4),
        carb = c(4, 4)
      ) |>
        structure(row.names = c("Mazda RX4", "Mazda RX4 Wag"))
    Code
      construct(tail(cars, 2))
    Output
      data.frame(speed = c(24, 25), dist = c(120, 85)) |>
        structure(row.names = 49:50)
    Code
      construct(head(cars, 2), read.table = TRUE)
    Output
      read.table(header = TRUE, text = "
      speed dist
      4.    2.
      4.    10.
      ")
    Code
      construct(transform(mtcars[1:2, 1:2], chr = c("a", "b"), int = 1:2),
      read.table = TRUE)
    Output
      read.table(header = TRUE, text = "
                      mpg cyl chr int
      'Mazda RX4'     21. 6.  a   1
      'Mazda RX4 Wag' 21. 6.  b   2
      ") |>
        structure(row.names = c("Mazda RX4", "Mazda RX4 Wag"))
    Code
      construct(head(iris, 2), read.table = TRUE)
    Output
      data.frame(
        Sepal.Length = c(5.1, 4.9),
        Sepal.Width = c(3.5, 3),
        Petal.Length = c(1.4, 1.4),
        Petal.Width = c(0.2, 0.2),
        Species = factor(c("setosa", "setosa"), levels = c("setosa", "versicolor", "virginica"))
      )


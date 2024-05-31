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
        carb = c(4, 4),
        row.names = c("Mazda RX4", "Mazda RX4 Wag")
      )
    Code
      construct(tail(cars, 2))
    Output
      data.frame(speed = c(24, 25), dist = c(120, 85), row.names = 49:50)
    Code
      construct(head(cars, 2), opts_data.frame(constructor = "read.table"))
    Output
      read.table(header = TRUE, text = "
      speed dist
         4.   2.
         4.  10.
      ")
    Code
      construct(head(cars, 2), opts_data.frame(constructor = "read.table"),
      one_liner = TRUE)
    Output
      read.table(header = TRUE, text = "\nspeed dist\n   4.   2.\n   4.  10.\n")
    Code
      construct(transform(mtcars[1:2, 1:2], chr = c("a", "b"), int = 1:2),
      opts_data.frame(constructor = "read.table"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      read.table(header = TRUE, text = "
                      mpg cyl chr int
          'Mazda RX4' 21.  6. 'a'  1L
      'Mazda RX4 Wag' 21.  6. 'b'  2L
      ")
    Code
      construct(head(iris, 2), opts_data.frame(constructor = "read.table"))
    Output
      data.frame(
        Sepal.Length = c(5.1, 4.9),
        Sepal.Width = c(3.5, 3),
        Petal.Length = c(1.4, 1.4),
        Petal.Width = c(0.2, 0.2),
        Species = factor(c("setosa", "setosa"), levels = c("setosa", "versicolor", "virginica"))
      )
    Code
      construct(data.frame(a = 1:2, b = 3:4)[2, ], opts_data.frame("read.table"))
    Output
      data.frame(a = 2L, b = 4L, row.names = 2L)
    Code
      construct(as.data.frame(tibble::tibble(a = 1:2, b = list(3, 4))))
    Output
      list(a = 1:2, b = list(3, 4)) |>
        structure(row.names = c(NA, -2L), class = "data.frame")
    Code
      construct(as.data.frame(tibble::tibble(a = 1:2, b = tibble::tibble(x = 3:4))))
    Output
      list(a = 1:2, b = tibble::tibble(x = 3:4)) |>
        structure(row.names = c(NA, -2L), class = "data.frame")
    Code
      construct(data.frame(a = 1, `a a` = 2, check.names = FALSE))
    Output
      data.frame(a = 1, `a a` = 2, check.names = FALSE)
    Code
      construct(data.frame(a = c(NA, NA), b = c(TRUE, NA), c = c(NA_character_, NA),
      d = c("a", NA), e = c(NA_integer_, NA), f = c(1L, NA), g = c(NA_real_, NA), h = c(
        1, NA)))
    Output
      data.frame(
        a = c(NA, NA),
        b = c(TRUE, NA),
        c = c(NA_character_, NA_character_),
        d = c("a", NA),
        e = c(NA_integer_, NA_integer_),
        f = c(1L, NA),
        g = c(NA_real_, NA_real_),
        h = c(1, NA)
      )
    Code
      construct(data.frame(a = I(list(2))))
    Output
      data.frame(a = I(list(2)))
    Code
      construct(data.frame(a = character()))
    Output
      data.frame(a = character(0))
    Code
      construct(head(cars, 2), opts_data.frame("list"))
    Output
      list(speed = c(4, 4), dist = c(2, 10)) |>
        structure(row.names = c(NA, -2L), class = "data.frame")
    Code
      construct(structure(list(V1 = NULL, V2 = NULL, V3 = NULL, V4 = NULL),
      row.names = c(NA, 0L), class = "data.frame"))
    Output
      list(V1 = NULL, V2 = NULL, V3 = NULL, V4 = NULL) |>
        structure(row.names = c(NA, 0L), class = "data.frame")
    Code
      construct(data.frame(a = "two words"), constructive::opts_data.frame(
        "read.table"))
    Output
      read.table(header = TRUE, text = "
                a
      'two words'
      ")
    Code
      constructive::construct(as.data.frame(list(row.names = 1:2)))
    Output
      list(row.names = 1:2) |>
        structure(class = "data.frame", row.names = c(NA, -2L))
    Code
      construct(data.frame(row.names = c("a", "b")))
    Output
      data.frame(row.names = c("a", "b"))


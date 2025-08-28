# data.frame

    Code
      construct(head(cars, 2))
    Output
      data.frame(speed = 4, dist = c(2, 10))
    Code
      construct(head(mtcars, 2))
    Output
      data.frame(
        mpg = 21,
        cyl = 6,
        disp = 160,
        hp = 110,
        drat = 3.9,
        wt = c(2.62, 2.875),
        qsec = c(16.46, 17.02),
        vs = 0,
        am = 1,
        gear = 4,
        carb = 4,
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
        Petal.Length = 1.4,
        Petal.Width = 0.2,
        Species = factor("setosa", levels = c("setosa", "versicolor", "virginica"))
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
        a = NA,
        b = c(TRUE, NA),
        c = NA_character_,
        d = c("a", NA),
        e = NA_integer_,
        f = c(1L, NA),
        g = NA_real_,
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
      construct(data.frame(row.names = c("a", "b")))
    Output
      data.frame(row.names = c("a", "b"))

# recycle in data frames

    Code
      construct(data.frame(a = 1:2, b = c(1, 1)))
    Output
      data.frame(a = 1:2, b = 1)
    Code
      construct(data.frame(a = c(1, 1), b = c(1, 1)))
    Output
      data.frame(a = c(1, 1), b = 1)
    Code
      construct(data.frame(a = 1:2, b = factor(c("a", "a"))))
    Output
      data.frame(a = 1:2, b = factor("a"))
    Code
      construct(data.frame(a = 1:2, b = as.Date(c("2000-01-01", "2000-01-01"))))
    Output
      data.frame(a = 1:2, b = as.Date("2000-01-01"))
    Code
      construct(data.frame(a = 1:2, b = structure(c(1, 1), foo = 1)))
    Output
      data.frame(
        a = 1:2,
        b = c(1, 1) |>
          structure(foo = 1)
      )

# duplicate names in data frames

    Code
      construct(data.frame(a = 1, a = 2, check.names = FALSE))
    Output
      data.frame(a = 1, a = 2, check.names = FALSE)

# non standard names in data frames

    Code
      construct(structure(data.frame(1), names = NULL))
    Output
      list(1) |>
        structure(class = "data.frame", row.names = c(NA, -1L))
    Code
      construct(structure(data.frame(1), names = ""))
    Output
      data.frame(1) |>
        structure(names = "")
    Code
      construct(structure(data.frame(1), names = NA))
    Output
      data.frame(1) |>
        structure(names = NA_character_)
    Code
      construct(structure(data.frame(1), names = "row.names"))
    Output
      data.frame(1) |>
        structure(names = "row.names")
    Code
      construct(structure(data.frame(1), names = "check.rows"))
    Output
      data.frame(1) |>
        structure(names = "check.rows")
    Code
      construct(structure(data.frame(1), names = "check.names"))
    Output
      data.frame(1) |>
        structure(names = "check.names")
    Code
      construct(structure(data.frame(1), names = "fix.empty.names"))
    Output
      data.frame(1) |>
        structure(names = "fix.empty.names")
    Code
      construct(structure(data.frame(1), names = "stringsAsFactors"))
    Output
      data.frame(1) |>
        structure(names = "stringsAsFactors")
    Code
      construct(structure(data.frame(1, 2), names = c("a", "")))
    Output
      data.frame(a = 1, 2) |>
        structure(names = c("a", ""))
    Code
      construct(structure(data.frame(1, 2), names = c("a", NA)))
    Output
      data.frame(a = 1, 2) |>
        structure(names = c("a", NA))
    Code
      construct(structure(data.frame(1, 2), names = c("a", "row.names")))
    Output
      data.frame(a = 1, 2) |>
        structure(names = c("a", "row.names"))
    Code
      construct(structure(data.frame(1, 2), names = c("a", "check.rows")))
    Output
      data.frame(a = 1, 2) |>
        structure(names = c("a", "check.rows"))
    Code
      construct(structure(data.frame(1, 2), names = c("a", "check.names")))
    Output
      data.frame(a = 1, 2) |>
        structure(names = c("a", "check.names"))
    Code
      construct(structure(data.frame(1, 2), names = c("a", "fix.empty.names")))
    Output
      data.frame(a = 1, 2) |>
        structure(names = c("a", "fix.empty.names"))
    Code
      construct(structure(data.frame(1, 2), names = c("a", "stringsAsFactors")))
    Output
      data.frame(a = 1, 2) |>
        structure(names = c("a", "stringsAsFactors"))


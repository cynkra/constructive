# one_liner

    Code
      construct(head(mtcars, 1), one_liner = TRUE)
    Output
      data.frame(mpg = 21, cyl = 6, disp = 160, hp = 110, drat = 3.9, wt = 2.62, qsec = 16.46, vs = 0, am = 1, gear = 4, carb = 4) |> structure(row.names = "Mazda RX4")
    Code
      construct(structure(1, foo = head(mtcars, 1)), one_liner = TRUE)
    Output
      1 |> structure(foo = data.frame(mpg = 21, cyl = 6, disp = 160, hp = 110, drat = 3.9, wt = 2.62, qsec = 16.46, vs = 0, am = 1, gear = 4, carb = 4) |> structure(row.names = "Mazda RX4"))
    Code
      construct(letters, one_liner = TRUE)
    Output
      c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
    Code
      construct(letters, one_liner = TRUE, max_atomic = 24)
    Output
      c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", +2)
    Code
      construct(c(1, 1:30), one_liner = TRUE)
    Output
      c(1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
    Code
      construct(c(1, 1:30), one_liner = TRUE, max_atomic = 24)
    Output
      c(1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, +7)


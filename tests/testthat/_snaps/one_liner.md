# one_liner

    Code
      construct(head(mtcars, 1), one_liner = TRUE)
    Output
      data.frame(mpg = 21, cyl = 6, disp = 160, hp = 110, drat = 3.9, wt = 2.62, qsec = 16.46, vs = 0, am = 1, gear = 4, carb = 4) |> structure(row.names = "Mazda RX4")
    Code
      construct(structure(1, foo = head(mtcars, 1)), one_liner = TRUE)
    Output
      1 |> structure(foo = data.frame(mpg = 21, cyl = 6, disp = 160, hp = 110, drat = 3.9, wt = 2.62, qsec = 16.46, vs = 0, am = 1, gear = 4, carb = 4) |> structure(row.names = "Mazda RX4"))


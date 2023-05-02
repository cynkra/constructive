# data.table

    Code
      construct(data.table::as.data.table(head(cars, 2)))
    Output
      data.table::data.table(speed = c(4, 4), dist = c(2, 10))
    Code
      construct(data.table::as.data.table(head(mtcars, 2)))
    Output
      data.table::data.table(
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
      )
    Code
      construct(data.table::data.table(head(mtcars, 2), key = "cyl"))
    Output
      data.table::data.table(
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
        key = "cyl"
      )


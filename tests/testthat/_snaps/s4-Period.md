# Period

    Code
      construct(lubridate::period(years = 1, days = 2))
    Output
      lubridate::period(years = 1, days = 2)
    Code
      construct(lubridate::period(years = c(1, 0), months = c(0, 2), seconds = c(0.5,
        0)))
    Output
      lubridate::period(years = c(1, 0), months = c(0, 2), seconds = c(0.5, 0))
    Code
      construct(lubridate::period(hours = c(1, NA), minutes = 30))
    Output
      lubridate::period(
        years = c(0, NA),
        months = c(0, NA),
        days = c(0, NA),
        hours = c(1, NA),
        minutes = c(30, NA),
        seconds = c(0, NA)
      )
    Code
      construct(lubridate::period(weeks = 1))
    Output
      lubridate::period(days = 7)
    Code
      construct(lubridate::period(seconds = c(0, 0)))
    Output
      lubridate::period(seconds = c(0, 0))
    Code
      construct(lubridate::period(-1.5, "second"))
    Output
      lubridate::period(seconds = -1.5)
    Code
      construct(lubridate::period())
    Output
      lubridate::period()
    Code
      construct(structure(lubridate::period(days = 1), foo = "bar"))
    Output
      lubridate::period(days = 1) |>
        structure(foo = "bar")
    Code
      construct(lubridate::period(years = 1, days = 2), opts_Period("period"))
    Output
      lubridate::period(years = 1, days = 2)
    Code
      construct(lubridate::period(years = 1, days = 2), opts_Period("next"))
    Output
      0 |>
        structure(
          year = 1,
          month = 0,
          day = 2,
          hour = 0,
          minute = 0,
          class = "Period" |>
            structure(package = "lubridate")
        ) |>
        asS4()


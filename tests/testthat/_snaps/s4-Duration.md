# Duration

    Code
      construct(lubridate::dweeks(2))
    Output
      lubridate::dweeks(2)
    Code
      construct(lubridate::ddays(3))
    Output
      lubridate::ddays(3)
    Code
      construct(lubridate::dhours(1))
    Output
      lubridate::dhours(1)
    Code
      construct(lubridate::dminutes(90))
    Output
      lubridate::dminutes(90)
    Code
      construct(lubridate::dseconds(1.5))
    Output
      lubridate::dseconds(1.5)
    Code
      construct(lubridate::dseconds(c(3600, 1)))
    Output
      lubridate::dseconds(c(3600, 1))
    Code
      construct(lubridate::dseconds(c(NA, 86400)))
    Output
      lubridate::ddays(c(NA, 1))
    Code
      construct(lubridate::dseconds(c(NA, NA)))
    Output
      lubridate::dseconds(c(NA_real_, NA_real_))
    Code
      construct(lubridate::dseconds(c(Inf, 86400)))
    Output
      lubridate::dseconds(c(Inf, 86400))
    Code
      construct(lubridate::dseconds(0))
    Output
      lubridate::dseconds(0)
    Code
      construct(lubridate::duration())
    Output
      lubridate::dseconds(numeric(0))
    Code
      construct(lubridate::dyears(1))
    Output
      lubridate::dhours(8766)
    Code
      construct(lubridate::dmonths(1))
    Output
      lubridate::dminutes(43830)
    Code
      construct(structure(lubridate::dminutes(1), foo = "bar"))
    Output
      lubridate::dminutes(1) |>
        structure(foo = "bar")
    Code
      construct(lubridate::ddays(1:3), opts_Duration("default"))
    Output
      lubridate::ddays(c(1, 2, 3))
    Code
      construct(lubridate::ddays(1:3), opts_Duration("dseconds"))
    Output
      lubridate::dseconds(c(86400, 172800, 259200))
    Code
      construct(lubridate::ddays(1:3), opts_Duration("duration"))
    Output
      lubridate::duration(c(86400, 172800, 259200))
    Code
      construct(lubridate::ddays(1:3), opts_Duration("next"))
    Output
      c(86400, 172800, 259200) |>
        structure(
          class = "Duration" |>
            structure(package = "lubridate")
        ) |>
        asS4()


# Duration

    Code
      construct(lubridate::dseconds(c(3.5, NA)))
    Output
      lubridate::dseconds(c(3.5, NA))
    Code
      construct(lubridate::ddays(1:3))
    Output
      lubridate::dseconds(c(86400, 172800, 259200))
    Code
      construct(lubridate::duration())
    Output
      lubridate::dseconds(numeric(0))
    Code
      construct(structure(lubridate::dminutes(1), foo = "bar"))
    Output
      lubridate::dseconds(60) |>
        structure(foo = "bar")
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


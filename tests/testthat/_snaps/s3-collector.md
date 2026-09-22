# collector

    Code
      construct(readr::col_character())
    Output
      readr::col_character()
    Code
      construct(readr::col_date())
    Output
      readr::col_date()
    Code
      construct(readr::col_date(format = "%d/%m/%Y"))
    Output
      readr::col_date(format = "%d/%m/%Y")
    Code
      construct(readr::col_datetime(format = "%Y-%m-%d %H:%M"))
    Output
      readr::col_datetime(format = "%Y-%m-%d %H:%M")
    Code
      construct(readr::col_double())
    Output
      readr::col_double()
    Code
      construct(readr::col_factor())
    Output
      readr::col_factor()
    Code
      construct(readr::col_factor(levels = c("a", "b"), ordered = TRUE, include_na = TRUE))
    Output
      readr::col_factor(levels = c("a", "b"), ordered = TRUE, include_na = TRUE)
    Code
      construct(readr::col_guess())
    Output
      readr::col_guess()
    Code
      construct(readr::col_integer())
    Output
      readr::col_integer()
    Code
      construct(readr::col_logical())
    Output
      readr::col_logical()
    Code
      construct(readr::col_number())
    Output
      readr::col_number()
    Code
      construct(readr::col_skip())
    Output
      readr::col_skip()
    Code
      construct(readr::col_time(format = "%H:%M"))
    Output
      readr::col_time(format = "%H:%M")
    Code
      construct(structure(readr::col_double(), foo = "bar"))
    Output
      readr::col_double() |>
        structure(foo = "bar")
    Code
      construct(structure(list(1), class = c("collector_double", "collector")))
    Output
      list(1) |>
        structure(class = c("collector_double", "collector"))
    Code
      construct(structure(list(), class = c("collector_unknown", "collector")))
    Output
      list() |>
        structure(class = c("collector_unknown", "collector"))
    Code
      construct(readr::col_date(format = "%Y"), opts_collector("next"))
    Output
      list(format = "%Y") |>
        structure(class = c("collector_date", "collector"))
    Code
      construct(readr::col_date(format = "%Y"), opts_collector("list"))
    Output
      list(format = "%Y") |>
        structure(class = c("collector_date", "collector"))


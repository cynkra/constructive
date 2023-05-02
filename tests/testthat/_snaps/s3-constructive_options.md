# constructive_options

    Code
      construct(opts_Date("as.Date"))
    Output
      constructive::opts_Date(origin = "1970-01-01")
    Code
      construct(opts_Date("as.Date"), opts_constructive_options("next"))
    Output
      list(constructor = "as.Date", origin = "1970-01-01") |>
        structure(class = c("constructive_options_Date", "constructive_options"))
    Code
      construct(opts_Date("new_date"))
    Output
      constructive::opts_Date("new_date", origin = "1970-01-01")
    Code
      construct(opts_Date(origin = "2020-01-01"))
    Output
      constructive::opts_Date(origin = "2020-01-01")


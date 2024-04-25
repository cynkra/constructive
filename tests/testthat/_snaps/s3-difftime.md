# difftime

    Code
      construct(as.difftime(1, units = "secs"))
    Output
      as.difftime(1, units = "secs")
    Code
      construct(as.difftime(structure(1, foo = 2), units = "mins"))
    Output
      as.difftime(1, units = "mins") |>
        structure(foo = 2)


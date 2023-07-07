# ts

    Code
      construct(ts(1:10, frequency = 4, start = c(1959, 2)))
    Output
      ts(1:10, frequency = 4, start = 1959.25)
    Code
      construct(ts(1:10, frequency = 4, start = c(1959, 2)), opts_ts("next"))
    Output
      1:10 |>
        structure(tsp = c(1959.25, 1961.5, 4), class = "ts")
    Code
      construct(ts(1:10, frequency = 4, start = c(1959, 2)), opts_ts("atomic"))
    Output
      1:10 |>
        structure(tsp = c(1959.25, 1961.5, 4), class = "ts")
    Code
      construct(ts(1:10, frequency = 7, start = c(12, 2)))
    Output
      ts(1:10, frequency = 7, start = 12.142857142857142)


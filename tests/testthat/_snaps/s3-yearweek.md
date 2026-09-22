# yearweek

    Code
      x <- tsibble::yearweek(c("2024 W01", "2020 W53"))
      construct(x)
    Output
      tsibble::yearweek(c("2024 W01", "2020 W53"))
    Code
      construct(x, opts_yearweek("next"))
    Output
      c(19723, 18624) |>
        structure(week_start = 1, class = c("yearweek", "vctrs_vctr"))
    Code
      construct(tsibble::yearweek(c("2024 W01", "2020 W53"), week_start = 7))
    Output
      tsibble::yearweek(c("2024 W01", "2020 W53"), week_start = 7)
    Code
      construct(tsibble::yearweek(as.Date(c("2024-01-01", NA))))
    Output
      tsibble::yearweek(as.Date(c("2024-01-01", NA)))
    Code
      construct(tsibble::yearweek(character()))
    Output
      tsibble::yearweek(as.Date(logical(0)))
    Code
      construct(stats::setNames(x, c("a", "b")))
    Output
      tsibble::yearweek(c("2024 W01", "2020 W53")) |>
        structure(names = c("a", "b"))
    Code
      construct(structure(19724, week_start = 1, class = c("yearweek", "vctrs_vctr")))
    Output
      19724 |>
        structure(week_start = 1, class = c("yearweek", "vctrs_vctr"))


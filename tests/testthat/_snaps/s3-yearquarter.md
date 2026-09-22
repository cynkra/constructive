# yearquarter

    Code
      x <- tsibble::yearquarter(c("2024 Q1", "2023 Q4"))
      construct(x)
    Output
      tsibble::yearquarter(c("2024 Q1", "2023 Q4"))
    Code
      construct(x, opts_yearquarter("next"))
    Output
      c(19723, 19631) |>
        structure(fiscal_start = 1, class = c("yearquarter", "vctrs_vctr"))
    Code
      construct(tsibble::yearquarter(c("2024 Q1", "2023 Q4"), fiscal_start = 4))
    Output
      tsibble::yearquarter(c("2024 Q1", "2023 Q4"), fiscal_start = 4)
    Code
      construct(tsibble::yearquarter(as.Date(c("2024-01-01", NA)), fiscal_start = 4))
    Output
      tsibble::yearquarter(as.Date(c("2024-01-01", NA)), fiscal_start = 4)
    Code
      construct(tsibble::yearquarter(as.Date(character())))
    Output
      tsibble::yearquarter(as.Date(logical(0)))
    Code
      construct(stats::setNames(x, c("a", "b")))
    Output
      tsibble::yearquarter(c("2024 Q1", "2023 Q4")) |>
        structure(names = c("a", "b"))
    Code
      construct(structure(19724, fiscal_start = 1, class = c("yearquarter",
        "vctrs_vctr")))
    Output
      19724 |>
        structure(fiscal_start = 1, class = c("yearquarter", "vctrs_vctr"))


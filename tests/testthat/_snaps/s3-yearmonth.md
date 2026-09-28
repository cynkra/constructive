# yearmonth

    Code
      x <- tsibble::yearmonth(c("2024 Jan", "2023 Dec"))
      construct(x)
    Output
      tsibble::yearmonth(c("2024 Jan", "2023 Dec"))
    Code
      construct(x, opts_yearmonth("next"))
    Output
      vctrs::new_vctr(c(19723, 19692), class = "yearmonth")
    Code
      construct(tsibble::yearmonth(as.Date(c("2024-01-01", NA))))
    Output
      tsibble::yearmonth(as.Date(c("2024-01-01", NA)))
    Code
      construct(tsibble::yearmonth(as.Date("1200-05-01")))
    Output
      tsibble::yearmonth(as.Date("1200-05-01"))
    Code
      construct(tsibble::yearmonth(character()))
    Output
      tsibble::yearmonth(character(0))
    Code
      construct(stats::setNames(x, c("a", "b")))
    Output
      tsibble::yearmonth(c("2024 Jan", "2023 Dec")) |>
        structure(names = c("a", "b"))
    Code
      construct(structure(19724, class = c("yearmonth", "vctrs_vctr")))
    Output
      vctrs::new_vctr(19724, class = "yearmonth")


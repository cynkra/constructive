# hms

    Code
      construct(hms::as_hms(c("12:34:56", "00:00:00", NA, "24:00:00")))
    Output
      hms::as_hms(c("12:34:56", "00:00:00", NA, "24:00:00"))
    Code
      construct(hms::new_hms(c(a = 0.5, b = 45296.25)))
    Output
      hms::as_hms(c("00:00:00.5", "12:34:56.25")) |>
        structure(names = c("a", "b"))
    Code
      construct(hms::hms(c(-5, 90000, 1 / 3, NA)))
    Output
      hms::hms(
        seconds = c(-5, 0, 0.3333333333333333, NA),
        minutes = c(0, 0, 0, NA),
        hours = c(0, 25, 0, NA)
      )
    Code
      construct(hms::hms(NA))
    Output
      hms::hms(seconds = NA_real_)
    Code
      construct(hms::hms())
    Output
      hms::hms()
    Code
      construct(hms::hms(hours = 1))
    Output
      hms::as_hms("01:00:00")
    Code
      construct(hms::as_hms(c("12:34:56", "01:00:05", NA)), opts_hms("hms"))
    Output
      hms::hms(seconds = c(56, 5, NA), minutes = c(34, 0, NA), hours = c(12, 1, NA))
    Code
      construct(hms::new_hms(c(a = 60, b = 120)), opts_hms("hms"))
    Output
      hms::hms(minutes = c(1, 2)) |>
        structure(names = c("a", "b"))
    Code
      construct(hms::hms(c(-3661, 1 / 3)), opts_hms("hms"))
    Output
      hms::hms(seconds = c(-1, 0.3333333333333333), minutes = c(-1, 0), hours = c(-1, 0))
    Code
      construct(hms::new_hms(c(a = 45296, b = NA)), opts_hms("new_hms"))
    Output
      hms::new_hms(c(a = 45296, b = NA))
    Code
      construct(hms::hms(45296), opts_hms("next"))
    Output
      as.difftime(45296, units = "secs") |>
        structure(class = c("hms", "difftime"))
    Code
      construct(hms::hms(45296), opts_hms("double"))
    Output
      45296 |>
        structure(units = "secs", class = c("hms", "difftime"))


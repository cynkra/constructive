# clock_year_day

    Code
      construct(clock::year_day(2024, 45))
    Output
      clock::year_day(2024, 45)
    Code
      construct(clock::year_day(2024, c(45, NA), 12, 30, 15, 5, subsecond_precision = "microsecond"))
    Output
      clock::year_day(
        c(2024, NA),
        c(45, NA),
        c(12, NA),
        c(30, NA),
        c(15, NA),
        c(5, NA),
        subsecond_precision = "microsecond"
      )
    Code
      construct(clock::year_day(2024, 45), opts_clock_year_day("next"))
    Output
      list(year = 2024L, day = 45L) |>
        structure(
          class = c("clock_year_day", "clock_calendar", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
          precision = 4L
        )
    Code
      construct(clock::year_day(2024, 45), opts_clock_year_day("list"))
    Output
      list(year = 2024L, day = 45L) |>
        structure(
          class = c("clock_year_day", "clock_calendar", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
          precision = 4L
        )


# clock_year_month_weekday

    Code
      construct(clock::year_month_weekday(2024, 1, clock::clock_weekdays$tuesday, 2))
    Output
      clock::year_month_weekday(2024, 1, 3, 2)
    Code
      construct(clock::year_month_weekday(2024, 1:2, c(3, NA), 2, 12, 30))
    Output
      clock::year_month_weekday(c(2024, NA), c(1, NA), c(3, NA), c(2, NA), c(12, NA), c(30, NA))
    Code
      construct(clock::year_month_weekday(2024, 1))
    Output
      clock::year_month_weekday(2024, 1)
    Code
      construct(clock::year_month_weekday(2024, 1, 3, 2),
      opts_clock_year_month_weekday("next"))
    Output
      list(year = 2024L, month = 1L, day = 3L, index = 2L) |>
        structure(
          class = c(
            "clock_year_month_weekday", "clock_calendar", "clock_rcrd", "vctrs_rcrd",
            "vctrs_vctr"
          ),
          precision = 4L
        )
    Code
      construct(clock::year_month_weekday(2024, 1, 3, 2),
      opts_clock_year_month_weekday("list"))
    Output
      list(year = 2024L, month = 1L, day = 3L, index = 2L) |>
        structure(
          class = c(
            "clock_year_month_weekday", "clock_calendar", "clock_rcrd", "vctrs_rcrd",
            "vctrs_vctr"
          ),
          precision = 4L
        )


# clock_year_quarter_day

    Code
      construct(clock::year_quarter_day(2024, 2, 3))
    Output
      clock::year_quarter_day(2024, 2, 3)
    Code
      construct(clock::year_quarter_day(2024, 2, 3, start = clock::clock_months$april))
    Output
      clock::year_quarter_day(2024, 2, 3, start = 4)
    Code
      construct(clock::year_quarter_day(2024, 1:4))
    Output
      clock::year_quarter_day(2024, seq(1, 4, by = 1))
    Code
      construct(clock::year_quarter_day(2024, 2, 3), opts_clock_year_quarter_day(
        "next"))
    Output
      list(year = 2024L, quarter = 2L, day = 3L) |>
        structure(
          class = c(
            "clock_year_quarter_day", "clock_calendar", "clock_rcrd", "vctrs_rcrd",
            "vctrs_vctr"
          ),
          precision = 4L,
          start = 1L
        )
    Code
      construct(clock::year_quarter_day(2024, 2, 3), opts_clock_year_quarter_day(
        "list"))
    Output
      list(year = 2024L, quarter = 2L, day = 3L) |>
        structure(
          class = c(
            "clock_year_quarter_day", "clock_calendar", "clock_rcrd", "vctrs_rcrd",
            "vctrs_vctr"
          ),
          precision = 4L,
          start = 1L
        )


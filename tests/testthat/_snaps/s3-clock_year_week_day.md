# clock_year_week_day

    Code
      construct(clock::year_week_day(2024, 2, 3))
    Output
      clock::year_week_day(2024, 2, 3)
    Code
      construct(clock::year_week_day(2024, 2, 3, start = clock::clock_weekdays$monday))
    Output
      clock::year_week_day(2024, 2, 3, start = 2)
    Code
      construct(clock::year_week_day(2024, 1:2, start = 3))
    Output
      clock::year_week_day(2024, c(1, 2), start = 3)
    Code
      construct(clock::year_week_day(2024, 2, 3), opts_clock_year_week_day("next"))
    Output
      list(year = 2024L, week = 2L, day = 3L) |>
        structure(
          class = c("clock_year_week_day", "clock_calendar", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
          precision = 4L,
          start = 1L
        )
    Code
      construct(clock::year_week_day(2024, 2, 3), opts_clock_year_week_day("list"))
    Output
      list(year = 2024L, week = 2L, day = 3L) |>
        structure(
          class = c("clock_year_week_day", "clock_calendar", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
          precision = 4L,
          start = 1L
        )


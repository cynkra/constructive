# clock_iso_year_week_day

    Code
      construct(clock::iso_year_week_day(2024, 2, 3))
    Output
      clock::iso_year_week_day(2024, 2, 3)
    Code
      construct(clock::iso_year_week_day(2024, 1:2))
    Output
      clock::iso_year_week_day(2024, c(1, 2))
    Code
      construct(clock::iso_year_week_day(2024, 2, 3), opts_clock_iso_year_week_day(
        "next"))
    Output
      list(year = 2024L, week = 2L, day = 3L) |>
        structure(
          class = c(
            "clock_iso_year_week_day", "clock_calendar", "clock_rcrd", "vctrs_rcrd",
            "vctrs_vctr"
          ),
          precision = 4L
        )
    Code
      construct(clock::iso_year_week_day(2024, 2, 3), opts_clock_iso_year_week_day(
        "list"))
    Output
      list(year = 2024L, week = 2L, day = 3L) |>
        structure(
          class = c(
            "clock_iso_year_week_day", "clock_calendar", "clock_rcrd", "vctrs_rcrd",
            "vctrs_vctr"
          ),
          precision = 4L
        )


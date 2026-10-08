# clock_year_month_day

    Code
      construct(clock::year_month_day(2024, 1, 15))
    Output
      clock::year_month_day(2024, 1, 15)
    Code
      construct(clock::year_month_day(2024, 1:3, 15))
    Output
      clock::year_month_day(2024, c(1, 2, 3), 15)
    Code
      construct(clock::year_month_day(c(2024, NA), 1, 15))
    Output
      clock::year_month_day(c(2024, NA), c(1, NA), c(15, NA))
    Code
      construct(clock::year_month_day(2024))
    Output
      clock::year_month_day(2024)
    Code
      construct(clock::year_month_day(integer()))
    Output
      clock::year_month_day(numeric(0))
    Code
      construct(clock::year_month_day(2024, 1, 15, 3, 4, 5, 123, subsecond_precision = "millisecond"))
    Output
      clock::year_month_day(2024, 1, 15, 3, 4, 5, 123, subsecond_precision = "millisecond")
    Code
      construct(structure(clock::year_month_day(2024, 1, 15), foo = "bar"))
    Output
      clock::year_month_day(2024, 1, 15) |>
        structure(foo = "bar")
    Code
      construct(clock::year_month_day(2024, 1, 15), opts_clock_year_month_day("next"))
    Output
      list(year = 2024L, month = 1L, day = 15L) |>
        structure(
          class = c("clock_year_month_day", "clock_calendar", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
          precision = 4L
        )
    Code
      construct(clock::year_month_day(2024, 1, 15), opts_clock_year_month_day("list"))
    Output
      list(year = 2024L, month = 1L, day = 15L) |>
        structure(
          class = c("clock_year_month_day", "clock_calendar", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
          precision = 4L
        )


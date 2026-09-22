# clock_naive_time

    Code
      construct(clock::as_naive_time(clock::year_month_day(2024, 1, 15)))
    Output
      clock::as_naive_time(clock::year_month_day(2024, 1, 15))
    Code
      construct(clock::as_naive_time(clock::year_month_day(2024, 1, 15, c(3, NA), 4,
      5)))
    Output
      clock::as_naive_time(
        clock::year_month_day(c(2024, NA), c(1, NA), c(15, NA), c(3, NA), c(4, NA), c(5, NA))
      )
    Code
      construct(clock::as_naive_time(clock::duration_nanoseconds(123456789)))
    Output
      clock::as_naive_time(
        clock::year_month_day(1970, 1, 1, 0, 0, 0, 123456789, subsecond_precision = "nanosecond")
      )
    Code
      construct(clock::as_naive_time(clock::duration_days(integer())))
    Output
      clock::as_naive_time(clock::year_month_day(numeric(0), numeric(0), numeric(0)))
    Code
      construct(clock::as_naive_time(clock::year_month_day(2024, 1, 15)),
      opts_clock_naive_time("next"))
    Output
      list(lower = 2147483648, upper = 19737) |>
        structure(
          class = c("clock_naive_time", "clock_time_point", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
          precision = 4L,
          clock = 1L
        )
    Code
      construct(clock::as_naive_time(clock::year_month_day(2024, 1, 15)),
      opts_clock_naive_time("list"))
    Output
      list(lower = 2147483648, upper = 19737) |>
        structure(
          class = c("clock_naive_time", "clock_time_point", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
          precision = 4L,
          clock = 1L
        )


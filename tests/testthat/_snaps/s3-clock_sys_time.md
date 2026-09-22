# clock_sys_time

    Code
      construct(clock::as_sys_time(clock::year_month_day(2024, 1, 15)))
    Output
      clock::as_sys_time(clock::year_month_day(2024, 1, 15))
    Code
      construct(clock::as_sys_time(clock::year_month_day(c(2024, NA), 1, 15, 3, 4, 5)))
    Output
      clock::as_sys_time(
        clock::year_month_day(c(2024, NA), c(1, NA), c(15, NA), c(3, NA), c(4, NA), c(5, NA))
      )
    Code
      construct(clock::as_sys_time(clock::duration_milliseconds(-1)))
    Output
      clock::as_sys_time(
        clock::year_month_day(1969, 12, 31, 23, 59, 59, 999, subsecond_precision = "millisecond")
      )
    Code
      construct(clock::as_sys_time(clock::year_month_day(2024, 1, 15)),
      opts_clock_sys_time("next"))
    Output
      list(lower = 2147483648, upper = 19737) |>
        structure(
          class = c("clock_sys_time", "clock_time_point", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
          precision = 4L,
          clock = 0L
        )
    Code
      construct(clock::as_sys_time(clock::year_month_day(2024, 1, 15)),
      opts_clock_sys_time("list"))
    Output
      list(lower = 2147483648, upper = 19737) |>
        structure(
          class = c("clock_sys_time", "clock_time_point", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
          precision = 4L,
          clock = 0L
        )


# clock_duration

    Code
      construct(clock::duration_days(3))
    Output
      clock::duration_days(3)
    Code
      construct(clock::duration_years(c(1, -2, NA)))
    Output
      clock::duration_years(c(1, -2, NA))
    Code
      construct(clock::duration_nanoseconds(integer()))
    Output
      clock::duration_nanoseconds(numeric(0))
    Code
      construct(clock::duration_cast(clock::duration_days(1), "nanosecond"))
    Output
      list(lower = 2147503764, upper = 2437873664) |>
        structure(
          class = c("clock_duration", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
          precision = 10L
        )
    Code
      construct(clock::duration_days(3), opts_clock_duration("next"))
    Output
      list(lower = 2147483648, upper = 3) |>
        structure(
          class = c("clock_duration", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
          precision = 4L
        )
    Code
      construct(clock::duration_days(3), opts_clock_duration("list"))
    Output
      list(lower = 2147483648, upper = 3) |>
        structure(
          class = c("clock_duration", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
          precision = 4L
        )


# clock_zoned_time

    Code
      naive <- clock::as_naive_time(clock::year_month_day(2024, 1, 15, 3, 4, 5))
      construct(clock::as_zoned_time(naive, "America/New_York"))
    Output
      clock::as_zoned_time(
        clock::as_naive_time(clock::year_month_day(2024, 1, 15, 3, 4, 5)),
        zone = "America/New_York"
      )
    Code
      construct(clock::as_zoned_time(clock::add_seconds(naive, c(0, NA)), "UTC"))
    Output
      clock::as_zoned_time(
        clock::as_naive_time(
          clock::year_month_day(c(2024, NA), c(1, NA), c(15, NA), c(3, NA), c(4, NA), c(5, NA))
        ),
        zone = "UTC"
      )
    Code
      ambiguous <- clock::as_naive_time(clock::year_month_day(2024, 11, 3, c(0, 1, 1),
      30, 0))
      construct(clock::as_zoned_time(ambiguous, "America/New_York", ambiguous = "latest"))
    Output
      clock::as_zoned_time(
        clock::as_naive_time(clock::year_month_day(2024, 11, 3, c(0, 1, 1), 30, 0)),
        zone = "America/New_York",
        ambiguous = "latest"
      )
    Code
      construct(clock::as_zoned_time(ambiguous, "America/New_York", ambiguous = c(
        "earliest", "earliest", "latest")))
    Output
      clock::as_zoned_time(
        clock::as_naive_time(clock::year_month_day(2024, 11, 3, c(0, 1, 1), 30, 0)),
        zone = "America/New_York",
        ambiguous = c("earliest", "earliest", "latest")
      )
    Code
      construct(clock::as_zoned_time(naive, "America/New_York"),
      opts_clock_zoned_time("next"))
    Output
      list(lower = 2147483648, upper = 1705305845) |>
        structure(
          class = c("clock_zoned_time", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
          precision = 7L,
          zone = "America/New_York"
        )
    Code
      construct(clock::as_zoned_time(naive, "America/New_York"),
      opts_clock_zoned_time("list"))
    Output
      list(lower = 2147483648, upper = 1705305845) |>
        structure(
          class = c("clock_zoned_time", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
          precision = 7L,
          zone = "America/New_York"
        )


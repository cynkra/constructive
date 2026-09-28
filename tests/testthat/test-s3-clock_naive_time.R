test_that("clock_naive_time", {
  skip_if_not_installed("clock")
  expect_snapshot({
    construct(clock::as_naive_time(clock::year_month_day(2024, 1, 15)))
    construct(clock::as_naive_time(clock::year_month_day(2024, 1, 15, c(3, NA), 4, 5)))
    construct(clock::as_naive_time(clock::duration_nanoseconds(123456789)))
    construct(clock::as_naive_time(clock::duration_days(integer())))
    construct(
      clock::as_naive_time(clock::year_month_day(2024, 1, 15)),
      opts_clock_naive_time("next")
    )
    construct(
      clock::as_naive_time(clock::year_month_day(2024, 1, 15)),
      opts_clock_naive_time("list")
    )
  })
})

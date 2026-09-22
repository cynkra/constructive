test_that("clock_sys_time", {
  skip_if_not_installed("clock")
  expect_snapshot({
    construct(clock::as_sys_time(clock::year_month_day(2024, 1, 15)))
    construct(clock::as_sys_time(clock::year_month_day(c(2024, NA), 1, 15, 3, 4, 5)))
    construct(clock::as_sys_time(clock::duration_milliseconds(-1)))
    construct(
      clock::as_sys_time(clock::year_month_day(2024, 1, 15)),
      opts_clock_sys_time("next")
    )
    construct(
      clock::as_sys_time(clock::year_month_day(2024, 1, 15)),
      opts_clock_sys_time("list")
    )
  })
})

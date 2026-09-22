test_that("clock_year_week_day", {
  skip_if_not_installed("clock")
  expect_snapshot({
    construct(clock::year_week_day(2024, 2, 3))
    construct(clock::year_week_day(2024, 2, 3, start = clock::clock_weekdays$monday))
    construct(clock::year_week_day(2024, 1:2, start = 3))
    construct(clock::year_week_day(2024, 2, 3), opts_clock_year_week_day("next"))
    construct(clock::year_week_day(2024, 2, 3), opts_clock_year_week_day("list"))
  })
})

test_that("clock_year_month_weekday", {
  skip_if_not_installed("clock")
  expect_snapshot({
    construct(clock::year_month_weekday(2024, 1, clock::clock_weekdays$tuesday, 2))
    construct(clock::year_month_weekday(2024, 1:2, c(3, NA), 2, 12, 30))
    construct(clock::year_month_weekday(2024, 1))
    construct(clock::year_month_weekday(2024, 1, 3, 2), opts_clock_year_month_weekday("next"))
    construct(clock::year_month_weekday(2024, 1, 3, 2), opts_clock_year_month_weekday("list"))
  })
})

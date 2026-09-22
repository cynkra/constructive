test_that("clock_year_day", {
  skip_if_not_installed("clock")
  expect_snapshot({
    construct(clock::year_day(2024, 45))
    construct(clock::year_day(2024, c(45, NA), 12, 30, 15, 5, subsecond_precision = "microsecond"))
    construct(clock::year_day(2024, 45), opts_clock_year_day("next"))
    construct(clock::year_day(2024, 45), opts_clock_year_day("list"))
  })
})

test_that("clock_year_quarter_day", {
  skip_if_not_installed("clock")
  expect_snapshot({
    construct(clock::year_quarter_day(2024, 2, 3))
    construct(clock::year_quarter_day(2024, 2, 3, start = clock::clock_months$april))
    construct(clock::year_quarter_day(2024, 1:4))
    construct(clock::year_quarter_day(2024, 2, 3), opts_clock_year_quarter_day("next"))
    construct(clock::year_quarter_day(2024, 2, 3), opts_clock_year_quarter_day("list"))
  })
})

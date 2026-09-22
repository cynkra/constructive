test_that("clock_year_month_day", {
  skip_if_not_installed("clock")
  expect_snapshot({
    construct(clock::year_month_day(2024, 1, 15))
    construct(clock::year_month_day(2024, 1:3, 15))
    construct(clock::year_month_day(c(2024, NA), 1, 15))
    construct(clock::year_month_day(2024))
    construct(clock::year_month_day(integer()))
    construct(clock::year_month_day(2024, 1, 15, 3, 4, 5, 123, subsecond_precision = "millisecond"))
    construct(structure(clock::year_month_day(2024, 1, 15), foo = "bar"))
    construct(clock::year_month_day(2024, 1, 15), opts_clock_year_month_day("next"))
    construct(clock::year_month_day(2024, 1, 15), opts_clock_year_month_day("list"))
  })
})

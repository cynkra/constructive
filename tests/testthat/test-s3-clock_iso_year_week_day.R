test_that("clock_iso_year_week_day", {
  skip_if_not_installed("clock")
  expect_snapshot({
    construct(clock::iso_year_week_day(2024, 2, 3))
    construct(clock::iso_year_week_day(2024, 1:2))
    construct(clock::iso_year_week_day(2024, 2, 3), opts_clock_iso_year_week_day("next"))
    construct(clock::iso_year_week_day(2024, 2, 3), opts_clock_iso_year_week_day("list"))
  })
})

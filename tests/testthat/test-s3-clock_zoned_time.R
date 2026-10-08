test_that("clock_zoned_time", {
  skip_if_not_installed("clock")
  expect_snapshot({
    naive <- clock::as_naive_time(clock::year_month_day(2024, 1, 15, 3, 4, 5))
    construct(clock::as_zoned_time(naive, "America/New_York"))
    construct(clock::as_zoned_time(clock::add_seconds(naive, c(0, NA)), "UTC"))
    # ambiguous local times at the end of daylight saving time
    ambiguous <- clock::as_naive_time(clock::year_month_day(2024, 11, 3, c(0, 1, 1), 30, 0))
    construct(clock::as_zoned_time(ambiguous, "America/New_York", ambiguous = "latest"))
    construct(clock::as_zoned_time(
      ambiguous, "America/New_York",
      ambiguous = c("earliest", "earliest", "latest")
    ))
    construct(clock::as_zoned_time(naive, "America/New_York"), opts_clock_zoned_time("next"))
    construct(clock::as_zoned_time(naive, "America/New_York"), opts_clock_zoned_time("list"))
  })
})

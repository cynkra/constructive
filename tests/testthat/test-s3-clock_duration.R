test_that("clock_duration", {
  skip_if_not_installed("clock")
  expect_snapshot({
    construct(clock::duration_days(3))
    construct(clock::duration_years(c(1, -2, NA)))
    construct(clock::duration_nanoseconds(integer()))
    # durations that don't fit in an integer fall back to "list"
    construct(clock::duration_cast(clock::duration_days(1), "nanosecond"))
    construct(clock::duration_days(3), opts_clock_duration("next"))
    construct(clock::duration_days(3), opts_clock_duration("list"))
  })
})

test_that("clock_weekday", {
  skip_if_not_installed("clock")
  expect_snapshot({
    construct(clock::weekday(1))
    construct(clock::weekday(c(1, 7, NA)))
    construct(clock::weekday(integer()))
    construct(clock::weekday(1:2), opts_clock_weekday("next"))
    construct(clock::weekday(1:2), opts_clock_weekday("integer"))
  })
})

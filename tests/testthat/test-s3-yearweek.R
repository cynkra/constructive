test_that("yearweek", {
  skip_if_not_installed("tsibble")
  expect_snapshot({
    x <- tsibble::yearweek(c("2024 W01", "2020 W53"))
    construct(x)
    construct(x, opts_yearweek("next"))
    construct(tsibble::yearweek(c("2024 W01", "2020 W53"), week_start = 7))
    # NA
    construct(tsibble::yearweek(as.Date(c("2024-01-01", NA))))
    # empty
    construct(tsibble::yearweek(character()))
    # names are dropped by `tsibble::yearweek()`
    construct(stats::setNames(x, c("a", "b")))
    # corrupted
    construct(structure(19724, week_start = 1, class = c("yearweek", "vctrs_vctr")))
  })
})

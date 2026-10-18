test_that("yearweek", {
  skip_if_not_installed("tsibble")
  expect_snapshot({
    x <- tsibble::yearweek(c("2024 W01", "2020 W53"))
    construct(x)
    construct(x, opts_yearweek("next"))
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

test_that("yearweek with week_start = 7, tsibble <= 1.2.0", {
  skip_if_not_installed("tsibble")
  skip_if(with_versions(tsibble > "1.2.0"))
  expect_snapshot({
    construct(tsibble::yearweek(c("2024 W01", "2020 W53"), week_start = 7))
  })
})

test_that("yearweek with week_start = 7, tsibble > 1.2.0", {
  # tsibble 1.2.0.9000 formats 2023-12-31 as "2023 W01", which parses as 2023-01-01
  skip_if_not_installed("tsibble")
  skip_if(with_versions(tsibble <= "1.2.0"))
  expect_snapshot({
    construct(tsibble::yearweek(c("2024 W01", "2020 W53"), week_start = 7))
  })
})

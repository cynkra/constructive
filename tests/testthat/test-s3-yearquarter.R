test_that("yearquarter", {
  skip_if_not_installed("tsibble")
  expect_snapshot({
    x <- tsibble::yearquarter(c("2024 Q1", "2023 Q4"))
    construct(x)
    construct(x, opts_yearquarter("next"))
    construct(tsibble::yearquarter(c("2024 Q1", "2023 Q4"), fiscal_start = 4))
    # NA
    construct(tsibble::yearquarter(as.Date(c("2024-01-01", NA)), fiscal_start = 4))
    # empty
    construct(tsibble::yearquarter(as.Date(character())))
    # names are dropped by `tsibble::yearquarter()`
    construct(stats::setNames(x, c("a", "b")))
    # corrupted
    construct(structure(19724, fiscal_start = 1, class = c("yearquarter", "vctrs_vctr")))
  })
})

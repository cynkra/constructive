test_that("yearmonth", {
  skip_if_not_installed("tsibble")
  expect_snapshot({
    x <- tsibble::yearmonth(c("2024 Jan", "2023 Dec"))
    construct(x)
    construct(x, opts_yearmonth("next"))
    # NA
    construct(tsibble::yearmonth(as.Date(c("2024-01-01", NA))))
    # year that can't be parsed
    construct(tsibble::yearmonth(as.Date("1200-05-01")))
    # empty
    construct(tsibble::yearmonth(character()))
    # names are dropped by `tsibble::yearmonth()`
    construct(stats::setNames(x, c("a", "b")))
    # corrupted
    construct(structure(19724, class = c("yearmonth", "vctrs_vctr")))
  })
})

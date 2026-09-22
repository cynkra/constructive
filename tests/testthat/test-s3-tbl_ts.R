test_that("tbl_ts", {
  skip_if_not_installed("tsibble")
  expect_snapshot({
    x <- tsibble::tsibble(
      k = c("a", "a", "b", "b"),
      t = tsibble::yearmonth(c("2024 Jan", "2024 Feb", "2024 Jan", "2024 Feb")),
      v = 1:4,
      key = k,
      index = t
    )
    construct(x)
    construct(x, opts_tbl_ts("as_tsibble"))
    construct(x, opts_tbl_ts("as_tsibble"), opts_tbl_df("tribble"))
    construct(x, opts_tbl_ts("next"))
    # no key, irregular, non syntactic names
    construct(tsibble::tsibble(t = c(1, 3, 10), `a b` = 1:3, index = t, regular = FALSE))
    # several keys, .drop = FALSE
    construct(tsibble::tsibble(
      k1 = c(1, 2), k2 = c("a", "b"), t = c(1, 1),
      key = c(k1, k2), index = t, .drop = FALSE
    ))
    # empty
    construct(tsibble::tsibble(t = numeric(), index = t))
    # column names conflicting with `tsibble()` args
    construct(tsibble::as_tsibble(tibble::tibble(key = 1:2, t = 1:2), index = t))
    # extra attributes
    construct(structure(x, foo = 1))
    # unsorted rows can't be reproduced by `tsibble()`
    construct(base::`[`(x, 4:1, ))
  })
})

test_that("grouped_ts", {
  skip_if_not_installed("tsibble")
  skip_if_not_installed("dplyr")
  expect_snapshot({
    x <- tsibble::tsibble(
      k = c("a", "a", "b", "b"),
      t = c(1, 2, 1, 2),
      v = 1:4,
      key = k,
      index = t
    )
    construct(dplyr::group_by(x, k))
    construct(dplyr::group_by(x, k), opts_grouped_ts("next"))
    construct(dplyr::group_by(x, k), opts_tbl_ts("as_tsibble"))
    construct(dplyr::group_by(x, k, .drop = FALSE))
    construct(tsibble::index_by(x, y = t %/% 2))
    construct(dplyr::group_by(tsibble::index_by(x, y = t %/% 2), k))
  })
})

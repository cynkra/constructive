test_that("tbl_df", {
  sys_time_1970 <- Sys.time()
  sys_time_1970[[1]] <- 0
  expect_snapshot({
    construct(dplyr::band_members)
    construct(dplyr::band_members, opts_tbl_df("next"))
    construct(dplyr::band_members, opts_tbl_df("next"), opts_data.frame("next"))
    construct(dplyr::band_members, opts_tbl_df(constructor = "tribble"))
    construct(dplyr::group_by(dplyr::band_members, band))
  })
})

test_that("tbl_df with `tribble = TRUE` falls back on tibble() if unsupported cols are found", {
  expect_snapshot({
    construct(tibble::tibble(a = 1:2, b = list(3, 4)), opts_tbl_df(constructor = "tribble"))
    construct(tibble::tibble(a = 1:2, b = tibble::tibble(x = 3:4)), opts_tbl_df(constructor = "tribble"))
  })
})

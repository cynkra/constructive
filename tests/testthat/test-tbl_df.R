test_that("tbl_df", {
  sys_time_1970 <- Sys.time()
  sys_time_1970[[1]] <- 0
  expect_snapshot({
    construct(dplyr::band_members)
    construct(dplyr::band_members, tribble = TRUE)
    construct(dplyr::group_by(dplyr::band_members, band))
  })
})


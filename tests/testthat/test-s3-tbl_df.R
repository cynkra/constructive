test_that("tbl_df", {
  sys_time_1970 <- Sys.time()
  sys_time_1970[[1]] <- 0
  expect_snapshot({
    construct(dplyr::band_members)
    construct(dplyr::band_members, opts_tbl_df("next"))
    construct(dplyr::band_members, opts_tbl_df("next"), opts_data.frame("next"))
    construct(dplyr::band_members, opts_tbl_df(constructor = "tribble"))
    construct(dplyr::band_members, opts_tbl_df(constructor = "tribble", justify = "right"))
    construct(tibble::tibble(a = as.Date("2001-01-01")), opts_tbl_df("tribble"))
    construct(dplyr::group_by(dplyr::band_members, band))
  })
})

test_that("tbl_df with `tribble = TRUE` falls back on tibble() if unsupported cols are found", {
  expect_snapshot({
    construct(tibble::tibble(a = 1:2, b = list(3, 4)), opts_tbl_df(constructor = "tribble"))
    construct(tibble::tibble(a = 1:2, b = tibble::tibble(x = 3:4)), opts_tbl_df(constructor = "tribble"))
  })
})

test_that("recycle in tibbles", {
  expect_snapshot({
    construct(tibble::tibble(a = 1:2, b = c(1, 1)))
    construct(tibble::tibble(a = c(1, 1), b = c(1, 1)))
    construct(tibble::tibble(a = 1:2, b = factor(c("a", "a"))))
    construct(tibble::tibble(a = 1:2, b = as.Date(c("2000-01-01", "2000-01-01"))))
  })
})

test_that("duplicate names in tibbles", {
  expect_snapshot({
    construct(tibble::tibble(a = 1, a =2, .name_repair = "minimal"))
  })
})

test_that("non standard names in tibbles", {
  expect_snapshot({
    construct(structure(tibble::tibble(1), names = NULL))
    construct(structure(tibble::tibble(1), names = ""))
    construct(structure(tibble::tibble(1), names = NA))
    construct(structure(tibble::tibble(1), names = ".rows"))
    construct(structure(tibble::tibble(1), names = ".name_repair"))
    construct(structure(tibble::tibble(1, 2), names = c("a", "")))
    construct(structure(tibble::tibble(1, 2), names = c("a", NA)))
    construct(structure(tibble::tibble(1, 2), names = c("a", ".rows")))
    construct(structure(tibble::tibble(1, 2), names = c("a", ".name_repair")))
  })
})

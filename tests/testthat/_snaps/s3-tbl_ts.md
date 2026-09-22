# tbl_ts

    Code
      x <- tsibble::tsibble(k = c("a", "a", "b", "b"), t = tsibble::yearmonth(c(
        "2024 Jan", "2024 Feb", "2024 Jan", "2024 Feb")), v = 1:4, key = k, index = t)
      construct(x)
    Output
      tsibble::tsibble(
        k = rep(c("a", "b"), each = 2L),
        t = tsibble::yearmonth(rep(c("2024 Jan", "2024 Feb"), 2)),
        v = 1:4,
        key = k,
        index = t
      )
    Code
      construct(x, opts_tbl_ts("as_tsibble"))
    Output
      tibble::tibble(
        k = rep(c("a", "b"), each = 2L),
        t = tsibble::yearmonth(rep(c("2024 Jan", "2024 Feb"), 2)),
        v = 1:4,
      ) |>
        tsibble::as_tsibble(key = k, index = t)
    Code
      construct(x, opts_tbl_ts("as_tsibble"), opts_tbl_df("tribble"))
    Output
      tibble::tribble(
        ~k,  ~t,                             ~v,
        "a", tsibble::yearmonth("2024 Jan"), 1L,
        "a", tsibble::yearmonth("2024 Feb"), 2L,
        "b", tsibble::yearmonth("2024 Jan"), 3L,
        "b", tsibble::yearmonth("2024 Feb"), 4L,
      ) |>
        tsibble::as_tsibble(key = k, index = t)
    Code
      construct(x, opts_tbl_ts("next"))
    Output
      tibble::tibble(
        k = rep(c("a", "b"), each = 2L),
        t = tsibble::yearmonth(rep(c("2024 Jan", "2024 Feb"), 2)),
        v = 1:4,
      ) |>
        structure(
          key = tibble::tibble(k = c("a", "b"), vctrs::list_of(1:2, 3:4, .ptype = integer(0))) |>
            structure(names = c("k", ".rows"), .drop = TRUE),
          index = "t" |>
            structure(ordered = TRUE),
          index2 = "t",
          interval = list(
            year = 0,
            quarter = 0,
            month = 1,
            week = 0,
            day = 0,
            hour = 0,
            minute = 0,
            second = 0,
            millisecond = 0,
            microsecond = 0,
            nanosecond = 0,
            unit = 0
          ) |>
            structure(.regular = TRUE, class = c("interval", "vctrs_rcrd", "vctrs_vctr")),
          class = c("tbl_ts", "tbl_df", "tbl", "data.frame")
        )
    Code
      construct(tsibble::tsibble(t = c(1, 3, 10), `a b` = 1:3, index = t, regular = FALSE))
    Output
      tsibble::tsibble(t = c(1, 3, 10), `a b` = 1:3, index = t, regular = FALSE)
    Code
      construct(tsibble::tsibble(k1 = c(1, 2), k2 = c("a", "b"), t = c(1, 1), key = c(
        k1, k2), index = t, .drop = FALSE))
    Output
      tsibble::tsibble(
        k1 = c(1, 2),
        k2 = c("a", "b"),
        t = c(1, 1),
        key = c(k1, k2),
        index = t,
        .drop = FALSE
      )
    Code
      construct(tsibble::tsibble(t = numeric(), index = t))
    Output
      tsibble::tsibble(t = numeric(0), index = t)
    Code
      construct(tsibble::as_tsibble(tibble::tibble(key = 1:2, t = 1:2), index = t))
    Output
      tibble::tibble(key = 1:2, t = 1:2) |>
        tsibble::as_tsibble(index = t)
    Code
      construct(structure(x, foo = 1))
    Output
      tsibble::tsibble(
        k = rep(c("a", "b"), each = 2L),
        t = tsibble::yearmonth(rep(c("2024 Jan", "2024 Feb"), 2)),
        v = 1:4,
        key = k,
        index = t
      ) |>
        structure(foo = 1)
    Code
      construct(base::`[`(x, 4:1, ))
    Output
      tibble::tibble(
        k = rep(c("b", "a"), each = 2L),
        t = tsibble::yearmonth(rep(c("2024 Feb", "2024 Jan"), 2)),
        v = 4:1,
      ) |>
        structure(
          key = tibble::tibble(k = c("a", "b"), vctrs::list_of(3:4, 1:2, .ptype = integer(0))) |>
            structure(names = c("k", ".rows"), .drop = TRUE),
          index = "t" |>
            structure(ordered = TRUE),
          index2 = "t",
          interval = list(
            year = 0,
            quarter = 0,
            month = 1,
            week = 0,
            day = 0,
            hour = 0,
            minute = 0,
            second = 0,
            millisecond = 0,
            microsecond = 0,
            nanosecond = 0,
            unit = 0
          ) |>
            structure(.regular = TRUE, class = c("interval", "vctrs_rcrd", "vctrs_vctr")),
          class = c("tbl_ts", "tbl_df", "tbl", "data.frame")
        )


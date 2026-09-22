# grouped_ts

    Code
      x <- tsibble::tsibble(k = c("a", "a", "b", "b"), t = c(1, 2, 1, 2), v = 1:4,
      key = k, index = t)
      construct(dplyr::group_by(x, k))
    Output
      tsibble::tsibble(k = rep(c("a", "b"), each = 2L), t = rep(c(1, 2), 2), v = 1:4, key = k, index = t) |>
        dplyr::group_by(k)
    Code
      construct(dplyr::group_by(x, k), opts_grouped_ts("next"))
    Output
      tsibble::tsibble(k = rep(c("a", "b"), each = 2L), t = rep(c(1, 2), 2), v = 1:4, key = k, index = t) |>
        structure(class = c("grouped_ts", "tbl_ts", "tbl_df", "tbl", "data.frame")) |>
        dplyr::group_by(k) |>
        structure(
          key = tibble::tibble(k = c("a", "b"), vctrs::list_of(1:2, 3:4, .ptype = integer(0))) |>
            structure(names = c("k", ".rows"), .drop = TRUE),
          index = "t" |>
            structure(ordered = TRUE),
          index2 = "t",
          interval = vctrs::new_rcrd(
            list(
              year = 0,
              quarter = 0,
              month = 0,
              week = 0,
              day = 0,
              hour = 0,
              minute = 0,
              second = 0,
              millisecond = 0,
              microsecond = 0,
              nanosecond = 0,
              unit = 1
            ),
            .regular = TRUE,
            class = "interval"
          ),
          class = c("grouped_ts", "grouped_df", "tbl_ts", "tbl_df", "tbl", "data.frame")
        )
    Code
      construct(dplyr::group_by(x, k), opts_tbl_ts("as_tsibble"))
    Output
      tibble::tibble(k = rep(c("a", "b"), each = 2L), t = rep(c(1, 2), 2), v = 1:4) |>
        tsibble::as_tsibble(key = k, index = t) |>
        dplyr::group_by(k)
    Code
      construct(dplyr::group_by(x, k, .drop = FALSE))
    Output
      tsibble::tsibble(k = rep(c("a", "b"), each = 2L), t = rep(c(1, 2), 2), v = 1:4, key = k, index = t) |>
        dplyr::group_by(k, .drop = FALSE)
    Code
      construct(tsibble::index_by(x, y = t %/% 2))
    Output
      tsibble::tsibble(
        k = rep(c("a", "b"), each = 2L),
        t = rep(c(1, 2), 2),
        v = 1:4,
        y = rep(c(0, 1), 2),
        key = k,
        index = t
      ) |>
        tsibble::index_by(y)
    Code
      construct(dplyr::group_by(tsibble::index_by(x, y = t %/% 2), k))
    Output
      tsibble::tsibble(
        k = rep(c("a", "b"), each = 2L),
        t = rep(c(1, 2), 2),
        v = 1:4,
        y = rep(c(0, 1), 2),
        key = k,
        index = t
      ) |>
        dplyr::group_by(k) |>
        tsibble::index_by(y)


# tbl_df

    Code
      construct(dplyr::band_members)
    Output
      tibble::tibble(name = c("Mick", "John", "Paul"), band = c("Stones", "Beatles", "Beatles"))
    Code
      construct(dplyr::band_members, opts_tbl_df("next"))
    Output
      data.frame(name = c("Mick", "John", "Paul"), band = c("Stones", "Beatles", "Beatles")) |>
        structure(class = c("tbl_df", "tbl", "data.frame"))
    Code
      construct(dplyr::band_members, opts_tbl_df("next"), opts_data.frame("next"))
    Output
      list(name = c("Mick", "John", "Paul"), band = c("Stones", "Beatles", "Beatles")) |>
        structure(class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -3L))
    Code
      construct(dplyr::band_members, opts_tbl_df(constructor = "tribble"))
    Output
      tibble::tribble(
        ~name,  ~band,
        "Mick", "Stones",
        "John", "Beatles",
        "Paul", "Beatles",
      )
    Code
      construct(dplyr::band_members, opts_tbl_df(constructor = "tribble", justify = "right"))
    Output
      tibble::tribble(
         ~name,     ~band,
        "Mick",  "Stones",
        "John", "Beatles",
        "Paul", "Beatles",
      )
    Code
      construct(tibble::tibble(a = as.Date("2001-01-01")), opts_tbl_df("tribble"))
    Output
      tibble::tribble(
        ~a,
        as.Date("2001-01-01"),
      )
    Code
      construct(dplyr::group_by(dplyr::band_members, band))
    Output
      tibble::tibble(name = c("Mick", "John", "Paul"), band = c("Stones", "Beatles", "Beatles")) |>
        dplyr::group_by(band)

# tbl_df with `tribble = TRUE` falls back on tibble() if unsupported cols are found

    Code
      construct(tibble::tibble(a = 1:2, b = list(3, 4)), opts_tbl_df(constructor = "tribble"))
    Output
      tibble::tibble(a = 1:2, b = list(3, 4))
    Code
      construct(tibble::tibble(a = 1:2, b = tibble::tibble(x = 3:4)), opts_tbl_df(
        constructor = "tribble"))
    Output
      tibble::tibble(
        a = 1:2,
        b = tibble::tribble(
          ~x,
          3L,
          4L,
        ),
      )

# recycle in tibbles

    Code
      construct(tibble::tibble(a = 1:2, b = c(1, 1)))
    Output
      tibble::tibble(a = 1:2, b = 1)
    Code
      construct(tibble::tibble(a = c(1, 1), b = c(1, 1)))
    Output
      tibble::tibble(a = 1, b = 1, .rows = 2L)
    Code
      construct(tibble::tibble(a = 1:2, b = factor(c("a", "a"))))
    Output
      tibble::tibble(a = 1:2, b = factor("a"))
    Code
      construct(tibble::tibble(a = 1:2, b = as.Date(c("2000-01-01", "2000-01-01"))))
    Output
      tibble::tibble(a = 1:2, b = as.Date("2000-01-01"))

# duplicate names in tibbles

    Code
      construct(tibble::tibble(a = 1, a = 2, .name_repair = "minimal"))
    Output
      tibble::tibble(a = 1, a = 2, .name_repair = "minimal")

# non standard names in tibbles

    Code
      construct(structure(tibble::tibble(1), names = NULL))
    Output
      list(1) |>
        structure(class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -1L))
    Code
      construct(structure(tibble::tibble(1), names = ""))
    Output
      tibble::tibble(1) |>
        structure(names = "")
    Code
      construct(structure(tibble::tibble(1), names = NA))
    Output
      tibble::tibble(1) |>
        structure(names = NA_character_)
    Code
      construct(structure(tibble::tibble(1), names = ".rows"))
    Output
      tibble::tibble(1) |>
        structure(names = ".rows")
    Code
      construct(structure(tibble::tibble(1), names = ".name_repair"))
    Output
      tibble::tibble(1) |>
        structure(names = ".name_repair")
    Code
      construct(structure(tibble::tibble(1, 2), names = c("a", "")))
    Output
      tibble::tibble(a = 1, 2) |>
        structure(names = c("a", ""))
    Code
      construct(structure(tibble::tibble(1, 2), names = c("a", NA)))
    Output
      tibble::tibble(a = 1, 2) |>
        structure(names = c("a", NA))
    Code
      construct(structure(tibble::tibble(1, 2), names = c("a", ".rows")))
    Output
      tibble::tibble(a = 1, 2) |>
        structure(names = c("a", ".rows"))
    Code
      construct(structure(tibble::tibble(1, 2), names = c("a", ".name_repair")))
    Output
      tibble::tibble(a = 1, 2) |>
        structure(names = c("a", ".name_repair"))


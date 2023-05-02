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
        structure(class = c("tbl_df", "tbl", "data.frame"), row.names = 1:3)
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


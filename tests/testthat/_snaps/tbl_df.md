# tbl_df

    Code
      construct(dplyr::band_members)
    Output
      tibble::tibble(name = c("Mick", "John", "Paul"), band = c("Stones", "Beatles", "Beatles"))
    Code
      construct(dplyr::band_members, tribble = TRUE)
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
      construct(tibble::tibble(a = 1:2, b = list(3, 4)), tribble = TRUE)
    Output
      tibble::tibble(a = 1:2, b = list(3, 4))
    Code
      construct(tibble::tibble(a = 1:2, b = tibble::tibble(x = 3:4)), tribble = TRUE)
    Output
      tibble::tibble(a = 1:2, b = tibble::tibble(x = 3:4))


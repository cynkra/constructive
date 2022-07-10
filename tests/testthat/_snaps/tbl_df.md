# tbl_df

    Code
      construct(dplyr::band_members)
    Output
      tibble::tibble(name = c("Mick", "John", "Paul"), band = c("Stones", "Beatles", "Beatles"))
    Code
      construct(dplyr::group_by(dplyr::band_members, band))
    Output
      tibble::tibble(name = c("Mick", "John", "Paul"), band = c("Stones", "Beatles", "Beatles")) |>
        dplyr::group_by(band)


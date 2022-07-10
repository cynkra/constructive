# grouped_df

    Code
      construct(dplyr::group_by(head(cars, 2), dist))
    Output
      tibble::tibble(speed = c(4, 4), dist = c(2, 10)) |>
        dplyr::group_by(dist)


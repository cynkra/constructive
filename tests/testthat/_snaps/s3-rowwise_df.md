# rowwise_df

    Code
      construct(dplyr::rowwise(head(cars, 2)))
    Output
      tibble::tibble(speed = 4, dist = c(2, 10)) |>
        dplyr::rowwise()
    Code
      construct(dplyr::rowwise(head(cars, 2), dist))
    Output
      tibble::tibble(speed = 4, dist = c(2, 10)) |>
        dplyr::rowwise(dist)


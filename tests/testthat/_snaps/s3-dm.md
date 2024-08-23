# dm

    Code
      construct(dm::dm(cars1 = head(cars, 2), cars2 = tail(cars, 2)), check = FALSE)
    Output
      dm::dm(
        cars1 = data.frame(speed = 4, dist = c(2, 10)),
        cars2 = data.frame(speed = c(24, 25), dist = c(120, 85), row.names = 49:50),
      )
    Code
      construct(dm::dm_pixarfilms(), data = "pixarfilms", check = FALSE)
    Output
      dm::dm(pixar_films, pixar_people, academy, box_office, genres, public_response) |>
        dm::dm_add_pk(pixar_films, "film") |>
        dm::dm_add_pk(academy, c("film", "award_type")) |>
        dm::dm_add_pk(box_office, "film") |>
        dm::dm_add_pk(genres, c("film", "genre")) |>
        dm::dm_add_pk(public_response, "film") |>
        dm::dm_add_fk(pixar_people, "film", pixar_films, "film") |>
        dm::dm_add_fk(academy, "film", pixar_films, "film") |>
        dm::dm_add_fk(box_office, "film", pixar_films, "film") |>
        dm::dm_add_fk(genres, "film", pixar_films, "film") |>
        dm::dm_add_fk(public_response, "film", pixar_films, "film") |>
        dm::dm_set_colors(
          `#5B9BD5FF` = "pixar_films",
          `#70AD47FF` = "pixar_people",
          `#ED7D31FF` = "academy",
          `#ED7D31FF` = "box_office",
          `#ED7D31FF` = "genres",
          `#ED7D31FF` = "public_response"
        )


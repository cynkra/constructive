test_that("dm", {
  expect_snapshot({
    # simple dm
    construct(dm::dm(cars1 = head(cars,2), cars2 = tail(cars,2)), check = FALSE)
    # skip because unstable
    # construct(dm::dm(cars1 = head(cars,2), cars2 = tail(cars,2)), opts_dm("next"))
    # construct(dm::dm(cars1 = head(cars,2), cars2 = tail(cars,2)), opts_dm("list"))

    pixar <- list(
      pixar_films = tibble::tribble(
        ~number, ~film,                 ~release_date,         ~run_time, ~film_rating,
        "1",     "Toy Story",           as.Date("1995-11-22"), 81,        "G",
        "2",     "A Bug's Life",        as.Date("1998-11-25"), 95,        "G",
        "3",     "Toy Story 2",         as.Date("1999-11-24"), 92,        "G",
      ),
      pixar_people = tibble::tribble(
        ~film,                 ~role_type,     ~name,
        "Toy Story",           "Director",     "John Lasseter",
        "Toy Story",           "Screenwriter", "Joel Cohen",
        "Toy Story",           "Screenwriter", "Alec Sokolow",
        "Toy Story",           "Screenwriter", "Andrew Stanton",
        "Toy Story",           "Screenwriter", "Joss Whedon",
        "A Bug's Life",        "Director",     "John Lasseter",
        "A Bug's Life",        "Screenwriter", "Donald McEnery",
        "A Bug's Life",        "Screenwriter", "Bob Shaw",
        "A Bug's Life",        "Screenwriter", "Andrew Stanton",
        "Toy Story 2",         "Director",     "John Lasseter",
        "Toy Story 2",         "Screenwriter", "Doug Chamberlin",
        "Toy Story 2",         "Screenwriter", "Rita Hsiao",
        "Toy Story 2",         "Screenwriter", "Andrew Stanton",
        "Toy Story 2",         "Screenwriter", "Chris Webb",
      ),
      academy = tibble::tribble(
        ~film,                 ~award_type,           ~status,
        "Toy Story",           "Animated Feature",    "Award not yet introduced",
        "Toy Story",           "Original Screenplay", "Nominated",
        "Toy Story",           "Adapted Screenplay",  "Ineligible",
        "Toy Story",           "Original Score",      "Nominated",
        "Toy Story",           "Original Song",       "Nominated",
        "Toy Story",           "Other",               "Won Special Achievement",
        "A Bug's Life",        "Animated Feature",    "Award not yet introduced",
        "A Bug's Life",        "Adapted Screenplay",  "Ineligible",
        "A Bug's Life",        "Original Score",      "Nominated",
        "Toy Story 2",         "Animated Feature",    "Award not yet introduced",
        "Toy Story 2",         "Original Screenplay", "Ineligible",
        "Toy Story 2",         "Original Song",       "Nominated",
      ),
      box_office = tibble::tribble(
        ~film,                 ~budget,  ~box_office_us_canada, ~box_office_other, ~box_office_worldwide,
        "Toy Story",           3e+07,    191796233,             181757800,         373554033,
        "A Bug's Life",        1.2e+08,  162798565,             200460294,         363258859,
        "Toy Story 2",         9e+07,    245852179,             251522597,         497374776,
      ),
      genres = tibble::tribble(
        ~film,                 ~genre,
        "Toy Story",           "Animation",
        "Toy Story",           "Adventure",
        "Toy Story",           "Comedy",
        "Toy Story",           "Family",
        "Toy Story",           "Fantasy",
        "A Bug's Life",        "Animation",
        "A Bug's Life",        "Adventure",
        "A Bug's Life",        "Comedy",
        "A Bug's Life",        "Family",
        "Toy Story 2",         "Animation",
        "Toy Story 2",         "Adventure",
        "Toy Story 2",         "Comedy",
        "Toy Story 2",         "Family",
        "Toy Story 2",         "Fantasy",
      ),
      public_response = tibble::tribble(
        ~film,                 ~rotten_tomatoes, ~metacritic, ~cinema_score, ~critics_choice,
        "Toy Story",           100,              95,          "A",           NA_real_,
        "A Bug's Life",        92,               77,          "A",           NA_real_,
        "Toy Story 2",         100,              88,          "A+",          100,
      )
    )

    dm_pixar <-
      dm::dm(
        pixar_films = pixar$pixar_films,
        pixar_people = pixar$pixar_people,
        academy = pixar$academy,
        box_office = pixar$box_office,
        genres = pixar$genres,
        public_response = pixar$public_response,
      ) |>
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

    # complex dm, using existing data
    construct(dm_pixar, check = FALSE, data = pixar)
  })
})

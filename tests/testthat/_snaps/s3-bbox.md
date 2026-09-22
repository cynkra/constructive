# bbox

    Code
      construct(sf::st_bbox(c(xmin = 1, ymin = 2, xmax = 3, ymax = 4)))
    Output
      sf::st_bbox(c(xmin = 1, ymin = 2, xmax = 3, ymax = 4))
    Code
      construct(sf::st_bbox(c(xmin = 1, ymin = 2, xmax = 3, ymax = 4), crs = 4326))
    Output
      sf::st_bbox(c(xmin = 1, ymin = 2, xmax = 3, ymax = 4), crs = 4326)
    Code
      construct(sf::st_bbox(sf::st_sfc()))
    Output
      sf::st_bbox(c(xmin = NA_real_, ymin = NA_real_, xmax = NA_real_, ymax = NA_real_))
    Code
      construct(sf::st_bbox(c(xmin = 1, ymin = 2, xmax = 3, ymax = 4)), opts_bbox(
        "next"))
    Output
      c(xmin = 1, ymin = 2, xmax = 3, ymax = 4) |>
        structure(class = "bbox", crs = sf::st_crs(NA))
    Code
      construct(structure(c(xmin = 1, ymin = 2, xmax = 3, ymax = 4), class = "bbox"))
    Output
      sf::st_bbox(c(xmin = 1, ymin = 2, xmax = 3, ymax = 4)) |>
        structure(crs = NULL)


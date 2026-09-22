# crs

    Code
      construct(sf::st_crs(4326))
    Output
      sf::st_crs(4326)
    Code
      construct(sf::st_crs("EPSG:2154"))
    Output
      sf::st_crs(2154)
    Code
      construct(sf::st_crs("OGC:CRS84"))
    Output
      sf::st_crs("OGC:CRS84")
    Code
      construct(sf::st_crs("+proj=longlat +datum=WGS84"))
    Output
      sf::st_crs("+proj=longlat +datum=WGS84")
    Code
      construct(sf::st_crs(NA))
    Output
      sf::st_crs(NA)
    Code
      construct(sf::st_crs(NA), opts_crs("next"))
    Output
      list(input = NA_character_, wkt = NA_character_) |>
        structure(class = "crs")


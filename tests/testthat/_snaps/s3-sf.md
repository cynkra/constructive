# sf

    Code
      construct(sf::st_sf(a = 1:2, b = c("x", "y"), geometry = geometry))
    Output
      sf::st_sf(
        a = 1:2,
        b = c("x", "y"),
        geometry = sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)), crs = 4326)
      )
    Code
      construct(sf::st_sf(geometry = geometry))
    Output
      sf::st_sf(
        geometry = sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)), crs = 4326)
      )
    Code
      construct(sf::st_sf(a = 1:2, geom = geometry, agr = c(a = "constant")))
    Output
      sf::st_sf(
        a = 1:2,
        geom = sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)), crs = 4326),
        agr = c(a = "constant")
      )
    Code
      construct(sf::st_sf(a = 1:2, g1 = geometry, g2 = geometry, sf_column_name = "g2"))
    Output
      sf::st_sf(
        a = 1:2,
        g1 = sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)), crs = 4326),
        g2 = sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)), crs = 4326),
        sf_column_name = "g2"
      )
    Code
      construct(sf::st_sf(a = 1:2, geometry = geometry, row.names = c("r1", "r2")))
    Output
      sf::st_sf(
        a = 1:2,
        geometry = sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)), crs = 4326),
        row.names = c("r1", "r2")
      )
    Code
      construct(sf::st_sf(a = integer(0), geometry = sf::st_sfc()))
    Output
      sf::st_sf(a = integer(0), geometry = sf::st_sfc())
    Code
      construct(sf::st_sf(tibble::tibble(geometry = geometry, a = 1:2)))
    Output
      sf::st_sf(
        tibble::tibble(
          geometry = sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)), crs = 4326),
          a = 1:2,
        )
      )
    Code
      construct(sf::st_sf(a = 1:2, geometry = geometry), opts_sf("next"))
    Output
      list(
        a = 1:2,
        geometry = sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)), crs = 4326)
      ) |>
        structure(
          row.names = c(NA, -2L),
          class = c("sf", "data.frame"),
          sf_column = "geometry",
          agr = factor(c(a = NA_character_), levels = c("constant", "aggregate", "identity"))
        )
    Code
      construct(sf::st_sf(data.frame(geometry = geometry, a = 1:2), sfc_last = FALSE))
    Output
      list(
        geometry = sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)), crs = 4326),
        a = 1:2
      ) |>
        structure(
          row.names = c(NA, -2L),
          class = c("sf", "data.frame"),
          sf_column = "geometry",
          agr = factor(c(a = NA_character_), levels = c("constant", "aggregate", "identity"))
        )


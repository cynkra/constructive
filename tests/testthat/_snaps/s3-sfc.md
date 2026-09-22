# sfc

    Code
      construct(sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4))))
    Output
      sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)))
    Code
      construct(sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)), crs = 4326))
    Output
      sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)), crs = 4326)
    Code
      construct(sf::st_sfc(sf::st_point(c(1, 2, 3)), crs = "EPSG:2154", precision = 100))
    Output
      sf::st_sfc(sf::st_point(c(1, 2, 3)), crs = 2154, precision = 100)
    Code
      construct(sf::st_sfc(sf::st_point(), sf::st_point(c(1, 2))))
    Output
      sf::st_sfc(sf::st_point(), sf::st_point(c(1, 2)))
    Code
      construct(sf::st_sfc(sf::st_point(c(1, 2)), sf::st_linestring(matrix(c(0, 1, 2,
        0, 1, 1), ncol = 2))))
    Output
      sf::st_sfc(
        sf::st_point(c(1, 2)),
        sf::st_linestring(matrix(c(0, 1, 2, 0, 1, 1), nrow = 3L, ncol = 2L))
      )
    Code
      construct(sf::st_sfc(sf::st_multipoint(matrix(1:8, 2), dim = "XYZM")))
    Output
      sf::st_sfc(sf::st_multipoint(matrix(1:8, nrow = 2L, ncol = 4L)))
    Code
      construct(sf::st_sfc(sf::st_geometrycollection(list(sf::st_point(c(1, 2))))))
    Output
      sf::st_sfc(sf::st_geometrycollection(list(sf::st_point(c(1, 2)))))
    Code
      construct(sf::st_sfc())
    Output
      sf::st_sfc()
    Code
      construct(sf::st_sfc(sf::st_point(c(1, 2))), opts_sfc("next"))
    Output
      list(sf::st_point(c(1, 2))) |>
        structure(
          class = c("sfc_POINT", "sfc"),
          precision = 0,
          bbox = sf::st_bbox(c(xmin = 1, ymin = 2, xmax = 1, ymax = 2)) |>
            structure(crs = NULL),
          crs = sf::st_crs(NA),
          n_empty = 0L
        )
    Code
      construct(structure(sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4))),
      names = c("a", "b")))
    Output
      sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4))) |>
        structure(names = c("a", "b"))
    Code
      construct(structure(sf::st_sfc(sf::st_point(c(1, 2))), n_empty = 3L))
    Output
      sf::st_sfc(sf::st_point(c(1, 2))) |>
        structure(n_empty = 3L)


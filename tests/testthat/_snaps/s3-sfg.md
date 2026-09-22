# sfg

    Code
      construct(sf::st_point(c(1, 2)))
    Output
      sf::st_point(c(1, 2))
    Code
      construct(sf::st_point(c(1, 2, 3)))
    Output
      sf::st_point(c(1, 2, 3))
    Code
      construct(sf::st_point(c(1, 2, 3), dim = "XYM"))
    Output
      sf::st_point(c(1, 2, 3), dim = "XYM")
    Code
      construct(sf::st_point(c(1, 2, 3, 4)))
    Output
      sf::st_point(seq(1, 4, by = 1))
    Code
      construct(sf::st_point())
    Output
      sf::st_point()
    Code
      construct(sf::st_multipoint(matrix(1:4, 2)))
    Output
      sf::st_multipoint(matrix(1:4, nrow = 2L, ncol = 2L))
    Code
      construct(sf::st_multipoint(matrix(1:6, 2), dim = "XYM"))
    Output
      sf::st_multipoint(matrix(1:6, nrow = 2L, ncol = 3L), dim = "XYM")
    Code
      construct(sf::st_linestring(matrix(c(0, 1, 2, 0, 1, 1), ncol = 2)))
    Output
      sf::st_linestring(matrix(c(0, 1, 2, 0, 1, 1), nrow = 3L, ncol = 2L))
    Code
      construct(sf::st_linestring())
    Output
      sf::st_linestring()
    Code
      construct(sf::st_multilinestring(list(matrix(c(0, 1, 2, 0, 1, 1), ncol = 2))))
    Output
      sf::st_multilinestring(list(matrix(c(0, 1, 2, 0, 1, 1), nrow = 3L, ncol = 2L)))
    Code
      construct(sf::st_polygon(list(matrix(c(0, 1, 1, 0, 0, 0, 1, 0), ncol = 2))))
    Output
      sf::st_polygon(list(matrix(c(0, 1, 1, 0, 0, 0, 1, 0), nrow = 4L, ncol = 2L)))
    Code
      construct(sf::st_multipolygon(list(list(matrix(c(0, 1, 1, 0, 0, 0, 1, 0), ncol = 2)))))
    Output
      sf::st_multipolygon(list(list(matrix(c(0, 1, 1, 0, 0, 0, 1, 0), nrow = 4L, ncol = 2L))))
    Code
      construct(sf::st_multipolygon())
    Output
      sf::st_multipolygon()
    Code
      construct(sf::st_geometrycollection(list(sf::st_point(c(1, 2)), sf::st_linestring(
        matrix(c(0, 1, 2, 0, 1, 1), ncol = 2)))))
    Output
      sf::st_geometrycollection(
        list(
          sf::st_point(c(1, 2)),
          sf::st_linestring(matrix(c(0, 1, 2, 0, 1, 1), nrow = 3L, ncol = 2L))
        )
      )
    Code
      construct(sf::st_geometrycollection())
    Output
      sf::st_geometrycollection()
    Code
      construct(sf::st_geometrycollection(dims = "XYZ"))
    Output
      sf::st_geometrycollection(dims = "XYZ")
    Code
      construct(sf::st_point(c(1, 2)), opts_sfg("next"))
    Output
      c(1, 2) |>
        structure(class = c("XY", "POINT", "sfg"))
    Code
      construct(structure(sf::st_point(c(1, 2)), foo = "bar"))
    Output
      sf::st_point(c(1, 2)) |>
        structure(foo = "bar")
    Code
      construct(structure(matrix(c(0, 1, 2, 0, 1, 0), 3), class = c("XY",
        "CIRCULARSTRING", "sfg")))
    Output
      matrix(c(0, 1, 2, 0, 1, 0), nrow = 3L, ncol = 2L) |>
        structure(class = c("XY", "CIRCULARSTRING", "sfg"))


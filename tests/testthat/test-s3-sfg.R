test_that("sfg", {
  skip_if_not_installed("sf")
  expect_snapshot({
    construct(sf::st_point(c(1, 2)))
    construct(sf::st_point(c(1, 2, 3)))
    construct(sf::st_point(c(1, 2, 3), dim = "XYM"))
    construct(sf::st_point(c(1, 2, 3, 4)))
    construct(sf::st_point())
    construct(sf::st_multipoint(matrix(1:4, 2)))
    construct(sf::st_multipoint(matrix(1:6, 2), dim = "XYM"))
    construct(sf::st_linestring(matrix(c(0, 1, 2, 0, 1, 1), ncol = 2)))
    construct(sf::st_linestring())
    construct(sf::st_multilinestring(list(matrix(c(0, 1, 2, 0, 1, 1), ncol = 2))))
    construct(sf::st_polygon(list(matrix(c(0, 1, 1, 0, 0, 0, 1, 0), ncol = 2))))
    construct(sf::st_multipolygon(list(list(matrix(c(0, 1, 1, 0, 0, 0, 1, 0), ncol = 2)))))
    construct(sf::st_multipolygon())
    construct(sf::st_geometrycollection(list(
      sf::st_point(c(1, 2)),
      sf::st_linestring(matrix(c(0, 1, 2, 0, 1, 1), ncol = 2))
    )))
    construct(sf::st_geometrycollection())
    construct(sf::st_geometrycollection(dims = "XYZ"))
    construct(sf::st_point(c(1, 2)), opts_sfg("next"))
    # extra attributes are repaired
    construct(structure(sf::st_point(c(1, 2)), foo = "bar"))
    # geometry types without constructors fall back to next constructor
    construct(structure(matrix(c(0, 1, 2, 0, 1, 0), 3), class = c("XY", "CIRCULARSTRING", "sfg")))
  })
})

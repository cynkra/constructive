test_that("sfc", {
  skip_if_not_installed("sf")
  skip_if(is.na(suppressWarnings(sf::st_crs(4326)$input)))
  expect_snapshot({
    construct(sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4))))
    construct(sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)), crs = 4326))
    construct(sf::st_sfc(sf::st_point(c(1, 2, 3)), crs = "EPSG:2154", precision = 100))
    construct(sf::st_sfc(sf::st_point(), sf::st_point(c(1, 2))))
    construct(sf::st_sfc(
      sf::st_point(c(1, 2)),
      sf::st_linestring(matrix(c(0, 1, 2, 0, 1, 1), ncol = 2))
    ))
    construct(sf::st_sfc(sf::st_multipoint(matrix(1:8, 2), dim = "XYZM")))
    construct(sf::st_sfc(sf::st_geometrycollection(list(sf::st_point(c(1, 2))))))
    construct(sf::st_sfc())
    construct(sf::st_sfc(sf::st_point(c(1, 2))), opts_sfc("next"))
    # attributes that don't match what st_sfc() computes are repaired
    construct(structure(sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4))), names = c("a", "b")))
    construct(structure(sf::st_sfc(sf::st_point(c(1, 2))), n_empty = 3L))
  })
})

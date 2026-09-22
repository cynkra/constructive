test_that("sf", {
  skip_if_not_installed("sf")
  skip_if_not_installed("tibble")
  skip_if(is.na(suppressWarnings(sf::st_crs(4326)$input)))
  geometry <- sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)), crs = 4326)
  expect_snapshot({
    construct(sf::st_sf(a = 1:2, b = c("x", "y"), geometry = geometry))
    construct(sf::st_sf(geometry = geometry))
    construct(sf::st_sf(a = 1:2, geom = geometry, agr = c(a = "constant")))
    construct(sf::st_sf(a = 1:2, g1 = geometry, g2 = geometry, sf_column_name = "g2"))
    construct(sf::st_sf(a = 1:2, geometry = geometry, row.names = c("r1", "r2")))
    construct(sf::st_sf(a = integer(0), geometry = sf::st_sfc()))
    construct(sf::st_sf(tibble::tibble(geometry = geometry, a = 1:2)))
    construct(sf::st_sf(a = 1:2, geometry = geometry), opts_sf("next"))
    # geometry columns that are not last can't be built by st_sf() from a data frame
    construct(sf::st_sf(data.frame(geometry = geometry, a = 1:2), sfc_last = FALSE))
  })
})

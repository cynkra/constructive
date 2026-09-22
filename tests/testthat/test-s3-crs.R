test_that("crs", {
  skip_if_not_installed("sf")
  skip_if(is.na(suppressWarnings(sf::st_crs(4326)$input)))
  expect_snapshot({
    construct(sf::st_crs(4326))
    construct(sf::st_crs("EPSG:2154"))
    construct(sf::st_crs("OGC:CRS84"))
    construct(sf::st_crs("+proj=longlat +datum=WGS84"))
    construct(sf::st_crs(NA))
    construct(sf::st_crs(NA), opts_crs("next"))
  })
})

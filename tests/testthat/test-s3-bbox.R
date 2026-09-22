test_that("bbox", {
  skip_if_not_installed("sf")
  skip_if(is.na(suppressWarnings(sf::st_crs(4326)$input)))
  expect_snapshot({
    construct(sf::st_bbox(c(xmin = 1, ymin = 2, xmax = 3, ymax = 4)))
    construct(sf::st_bbox(c(xmin = 1, ymin = 2, xmax = 3, ymax = 4), crs = 4326))
    construct(sf::st_bbox(sf::st_sfc()))
    construct(sf::st_bbox(c(xmin = 1, ymin = 2, xmax = 3, ymax = 4)), opts_bbox("next"))
    # a missing crs attribute is repaired
    construct(structure(c(xmin = 1, ymin = 2, xmax = 3, ymax = 4), class = "bbox"))
  })
})

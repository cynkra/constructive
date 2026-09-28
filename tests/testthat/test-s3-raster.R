test_that("raster", {
  expect_snapshot({
    construct(as.raster(matrix(c("red", "blue", "green", "white"), 2)))
    construct(as.raster(matrix(c(0, 0.5, 1, 0.2, NA, 1), 2)))
    construct(as.raster(matrix(character(), 0, 2)))
    construct(as.raster(matrix(c("red", "blue", "green", "white"), 2)), opts_raster("next"))
  })
})

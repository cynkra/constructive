# raster

    Code
      construct(as.raster(matrix(c("red", "blue", "green", "white"), 2)))
    Output
      as.raster(matrix(c("red", "blue", "green", "white"), nrow = 2L, ncol = 2L))
    Code
      construct(as.raster(matrix(c(0, 0.5, 1, 0.2, NA, 1), 2)))
    Output
      as.raster(
        matrix(
          c("#000000", "#808080", "#FFFFFF", "#333333", NA, "#FFFFFF"),
          nrow = 2L,
          ncol = 3L
        )
      )
    Code
      construct(as.raster(matrix(character(), 0, 2)))
    Output
      as.raster(matrix(character(0), nrow = 0L, ncol = 2L))
    Code
      construct(as.raster(matrix(c("red", "blue", "green", "white"), 2)), opts_raster(
        "next"))
    Output
      matrix(c("red", "green", "blue", "white"), nrow = 2L, ncol = 2L) |>
        structure(class = "raster")


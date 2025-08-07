# CoordFixed

    Code
      construct(ggplot2::coord_fixed(ratio = 1))
    Output
      ggplot2::coord_fixed()
    Code
      construct(ggplot2::coord_fixed(ratio = 5))
    Output
      ggplot2::coord_fixed(ratio = 5)
    Code
      construct(ggplot2::coord_fixed(ratio = 1 / 5))
    Output
      ggplot2::coord_fixed(ratio = 0.2)

# CoordFlip

    Code
      construct(ggplot2::coord_flip())
    Output
      ggplot2::coord_flip()

# CoordMap

    Code
      construct(ggplot2::coord_map())
    Output
      ggplot2::coord_map()
    Code
      construct(ggplot2::coord_map("azequalarea", orientation = c(-36.92, 174.6, 0)))
    Output
      ggplot2::coord_map(projection = "azequalarea", orientation = c(-36.92, 174.6, 0))
    Code
      construct(ggplot2::coord_quickmap())
    Output
      ggplot2::coord_quickmap()
    Code
      construct(ggplot2::coord_map("gilbert"))
    Output
      ggplot2::coord_map(projection = "gilbert")
    Code
      construct(ggplot2::coord_map("conic", lat0 = 30))
    Output
      ggplot2::coord_map(projection = "conic", lat0 = 30)

# CoordPolar

    Code
      construct(ggplot2::coord_polar(theta = "y"))
    Output
      ggplot2::coord_polar(theta = "y")
    Code
      construct(ggplot2::coord_polar())
    Output
      ggplot2::coord_polar()
    Code
      construct(ggplot2::coord_polar("y", start = pi / 3))
    Output
      ggplot2::coord_polar(theta = "y", start = 1.0471975511965976)

# CoordSf

    Code
      construct(ggplot2::coord_sf(default_crs = sf::st_crs(4326)), data = list(crs = sf::st_crs(
        4326)))
    Output
      ggplot2::coord_sf(default_crs = crs)

# CoordTransform > v3.5.2

    Code
      construct(ggplot2::coord_transform(x = "log10", y = "log10"), check = FALSE)
    Output
      ggplot2::coord_transform(x = "log-10", y = "log-10")
    Code
      construct(ggplot2::coord_transform(x = scales::exp_trans(10), y = scales::exp_trans(
        10)), check = FALSE)
    Output
      ggplot2::coord_transform(x = "power-10", y = "power-10")


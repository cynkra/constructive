# CoordCartesian

    Code
      construct(ggplot2::scale_x_continuous(limits = c(325, 500)))
    Output
      ggplot2::xlim(325, 500)
    Code
      construct(ggplot2::coord_cartesian(xlim = c(325, 500)))
    Output
      ggplot2::coord_cartesian(xlim = c(325, 500))
    Code
      construct(ggplot2::coord_cartesian(xlim = c(325, 500), expand = FALSE))
    Output
      ggplot2::coord_cartesian(xlim = c(325, 500), expand = FALSE)
    Code
      construct(ggplot2::coord_cartesian(expand = FALSE))
    Output
      ggplot2::coord_cartesian(expand = FALSE)

---

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
    Code
      construct(ggplot2::coord_fixed(xlim = c(15, 30)))
    Output
      ggplot2::coord_fixed(xlim = c(15, 30))

# CoordFlip

    Code
      construct(ggplot2::coord_flip())
    Output
      ggplot2::coord_flip()

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


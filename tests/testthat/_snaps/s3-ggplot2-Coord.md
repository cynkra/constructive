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
      construct(ggplot2::coord_sf(default_crs = sf::st_crs(4326)))
    Output
      ggplot2::coord_sf(
        default_crs = list(
          input = "EPSG:4326",
          wkt = "GEOGCRS[\"WGS 84\",\n    ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n        MEMBER[\"World Geodetic System 1984 (Transit)\"],\n        MEMBER[\"World Geodetic System 1984 (G730)\"],\n        MEMBER[\"World Geodetic System 1984 (G873)\"],\n        MEMBER[\"World Geodetic System 1984 (G1150)\"],\n        MEMBER[\"World Geodetic System 1984 (G1674)\"],\n        MEMBER[\"World Geodetic System 1984 (G1762)\"],\n        MEMBER[\"World Geodetic System 1984 (G2139)\"],\n        ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n            LENGTHUNIT[\"metre\",1]],\n        ENSEMBLEACCURACY[2.0]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    USAGE[\n        SCOPE[\"Horizontal component of 3D system.\"],\n        AREA[\"World.\"],\n        BBOX[-90,-180,90,180]],\n    ID[\"EPSG\",4326]]"
        ) %>%
          structure(class = "crs")
      )


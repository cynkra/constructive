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

---

    Code
      construct(ggplot2::coord_trans(x = "log10", y = "log10"), check = FALSE)
    Output
      ggplot2::coord_trans(
        x = scales::trans_new(
          name = "log-10",
          transform = (function(x) log(x, base)) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          inverse = (function(x) base^x) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          breaks = (function(x, n = n_default) {
            raw_rng <- suppressWarnings(range(x, na.rm = TRUE))
            if (any(!is.finite(raw_rng))) {
              return(numeric())
            }
            rng <- log(raw_rng, base = base)
            min <- floor(rng[1])
            max <- ceiling(rng[2])
            if (max == min) {
              return(base^min)
            }
            by <- floor((max - min) / n) + 1
            breaks <- base^seq(min, max, by = by)
            relevant_breaks <- base^rng[1] <= breaks & breaks <= base^rng[2]
            if (sum(relevant_breaks) >= (n - 2)) {
              return(breaks)
            }
            while (by > 1) {
              by <- by - 1
              breaks <- base^seq(min, max, by = by)
              relevant_breaks <- base^rng[1] <= breaks & breaks <= base^rng[2]
              if (sum(relevant_breaks) >= (n - 2)) {
                return(breaks)
              }
            }
            log_sub_breaks(rng, n = n, base = base)
          }) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          minor_breaks = (function(b, limits, n) {
            b <- b[!is.na(b)]
            if (length(b) < 2) {
              return()
            }
            bd <- diff(b)[1]
            if (!reverse) {
              if (min(limits) < min(b)) b <- c(b[1] - bd, b)
              if (max(limits) > max(b)) b <- c(b, b[length(b)] + bd)
            } else {
              if (max(limits) > max(b)) b <- c(b[1] - bd, b)
              if (min(limits) < min(b)) b <- c(b, b[length(b)] + bd)
            }
            seq_between <- function(a, b) {
              seq(a, b, length.out = n + 1)[-(n + 1)]
            }
            breaks <- unlist(Map(seq_between, b[-length(b)], b[-1]))
            breaks <- c(breaks, b[length(b)])
            breaks
          }) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          format = (function(x) {
            if (!is.null(names(x))) {
              return(names(x))
            }
            ret <- format(x, ..., trim = TRUE, justify = "left")
            ret[is.na(x)] <- NA
            ret
          }) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          domain = c(1e-100, Inf)
        ),
        y = scales::trans_new(
          name = "log-10",
          transform = (function(x) log(x, base)) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          inverse = (function(x) base^x) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          breaks = (function(x, n = n_default) {
            raw_rng <- suppressWarnings(range(x, na.rm = TRUE))
            if (any(!is.finite(raw_rng))) {
              return(numeric())
            }
            rng <- log(raw_rng, base = base)
            min <- floor(rng[1])
            max <- ceiling(rng[2])
            if (max == min) {
              return(base^min)
            }
            by <- floor((max - min) / n) + 1
            breaks <- base^seq(min, max, by = by)
            relevant_breaks <- base^rng[1] <= breaks & breaks <= base^rng[2]
            if (sum(relevant_breaks) >= (n - 2)) {
              return(breaks)
            }
            while (by > 1) {
              by <- by - 1
              breaks <- base^seq(min, max, by = by)
              relevant_breaks <- base^rng[1] <= breaks & breaks <= base^rng[2]
              if (sum(relevant_breaks) >= (n - 2)) {
                return(breaks)
              }
            }
            log_sub_breaks(rng, n = n, base = base)
          }) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          minor_breaks = (function(b, limits, n) {
            b <- b[!is.na(b)]
            if (length(b) < 2) {
              return()
            }
            bd <- diff(b)[1]
            if (!reverse) {
              if (min(limits) < min(b)) b <- c(b[1] - bd, b)
              if (max(limits) > max(b)) b <- c(b, b[length(b)] + bd)
            } else {
              if (max(limits) > max(b)) b <- c(b[1] - bd, b)
              if (min(limits) < min(b)) b <- c(b, b[length(b)] + bd)
            }
            seq_between <- function(a, b) {
              seq(a, b, length.out = n + 1)[-(n + 1)]
            }
            breaks <- unlist(Map(seq_between, b[-length(b)], b[-1]))
            breaks <- c(breaks, b[length(b)])
            breaks
          }) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          format = (function(x) {
            if (!is.null(names(x))) {
              return(names(x))
            }
            ret <- format(x, ..., trim = TRUE, justify = "left")
            ret[is.na(x)] <- NA
            ret
          }) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          domain = c(1e-100, Inf)
        )
      )
    Code
      construct(ggplot2::coord_trans(x = scales::exp_trans(10), y = scales::exp_trans(
        10)), check = FALSE)
    Output
      ggplot2::coord_trans(
        x = scales::trans_new(
          name = "power-10",
          transform = (function(x) base^x) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          inverse = (function(x) log(x, base = base)) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          breaks = (function(x, n = n_default) {
            x <- x[is.finite(x)]
            if (length(x) == 0) {
              return(numeric())
            }
            rng <- range(x)
            labeling::extended(rng[1], rng[2], n, ...)
          }) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          minor_breaks = (function(b, limits, n) {
            b <- b[!is.na(b)]
            if (length(b) < 2) {
              return()
            }
            bd <- diff(b)[1]
            if (!reverse) {
              if (min(limits) < min(b)) b <- c(b[1] - bd, b)
              if (max(limits) > max(b)) b <- c(b, b[length(b)] + bd)
            } else {
              if (max(limits) > max(b)) b <- c(b[1] - bd, b)
              if (min(limits) < min(b)) b <- c(b, b[length(b)] + bd)
            }
            seq_between <- function(a, b) {
              seq(a, b, length.out = n + 1)[-(n + 1)]
            }
            breaks <- unlist(Map(seq_between, b[-length(b)], b[-1]))
            breaks <- c(breaks, b[length(b)])
            breaks
          }) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          format = (function(x) {
            if (!is.null(names(x))) {
              return(names(x))
            }
            ret <- format(x, ..., trim = TRUE, justify = "left")
            ret[is.na(x)] <- NA
            ret
          }) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          domain = c(-Inf, Inf)
        ),
        y = scales::trans_new(
          name = "power-10",
          transform = (function(x) base^x) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          inverse = (function(x) log(x, base = base)) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          breaks = (function(x, n = n_default) {
            x <- x[is.finite(x)]
            if (length(x) == 0) {
              return(numeric())
            }
            rng <- range(x)
            labeling::extended(rng[1], rng[2], n, ...)
          }) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          minor_breaks = (function(b, limits, n) {
            b <- b[!is.na(b)]
            if (length(b) < 2) {
              return()
            }
            bd <- diff(b)[1]
            if (!reverse) {
              if (min(limits) < min(b)) b <- c(b[1] - bd, b)
              if (max(limits) > max(b)) b <- c(b, b[length(b)] + bd)
            } else {
              if (max(limits) > max(b)) b <- c(b[1] - bd, b)
              if (min(limits) < min(b)) b <- c(b, b[length(b)] + bd)
            }
            seq_between <- function(a, b) {
              seq(a, b, length.out = n + 1)[-(n + 1)]
            }
            breaks <- unlist(Map(seq_between, b[-length(b)], b[-1]))
            breaks <- c(breaks, b[length(b)])
            breaks
          }) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          format = (function(x) {
            if (!is.null(names(x))) {
              return(names(x))
            }
            ret <- format(x, ..., trim = TRUE, justify = "left")
            ret[is.na(x)] <- NA
            ret
          }) |>
            (`environment<-`)(constructive::.env("0x000000000", parents = "namespace:scales")),
          domain = c(-Inf, Inf)
        )
      )

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
      ggplot2::coord_polar(theta = "y", start = 1.047197551196597631318)

# CoordSf

    Code
      construct(ggplot2::coord_sf(default_crs = sf::st_crs(4326)))
    Output
      ggplot2::coord_sf(
        default_crs = list(
          input = "EPSG:4326",
          wkt = "GEOGCRS[\"WGS 84\",\n    ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n        MEMBER[\"World Geodetic System 1984 (Transit)\"],\n        MEMBER[\"World Geodetic System 1984 (G730)\"],\n        MEMBER[\"World Geodetic System 1984 (G873)\"],\n        MEMBER[\"World Geodetic System 1984 (G1150)\"],\n        MEMBER[\"World Geodetic System 1984 (G1674)\"],\n        MEMBER[\"World Geodetic System 1984 (G1762)\"],\n        MEMBER[\"World Geodetic System 1984 (G2139)\"],\n        ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n            LENGTHUNIT[\"metre\",1]],\n        ENSEMBLEACCURACY[2.0]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    USAGE[\n        SCOPE[\"Horizontal component of 3D system.\"],\n        AREA[\"World.\"],\n        BBOX[-90,-180,90,180]],\n    ID[\"EPSG\",4326]]"
        ) |>
          structure(class = "crs")
      )


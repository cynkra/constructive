test_that("CoordCartesian", {
  expect_snapshot({
    construct(ggplot2::scale_x_continuous(limits = c(325, 500)))
    construct(ggplot2::coord_cartesian(xlim = c(325, 500)))
    construct(ggplot2::coord_cartesian(xlim = c(325, 500), expand = FALSE))
    construct(ggplot2::coord_cartesian(expand = FALSE))
  })
})

test_that("CoordFixed", {
  expect_snapshot({
    construct(ggplot2::coord_fixed(ratio = 1))
    construct(ggplot2::coord_fixed(ratio = 5))
    construct(ggplot2::coord_fixed(ratio = 1/5))
    construct(ggplot2::coord_fixed(xlim = c(15, 30)))
  })
})

test_that("CoordFlip", {
  expect_snapshot({
    construct(ggplot2::coord_flip())
  })
})

test_that("CoordCartesian", {
  expect_snapshot({
    construct(ggplot2::coord_map())
    construct(ggplot2::coord_map("azequalarea", orientation = c(-36.92, 174.6, 0)))
    construct(ggplot2::coord_quickmap())
    construct(ggplot2::coord_map("gilbert"))
    construct(ggplot2::coord_map("conic", lat0 = 30))
  })
})

test_that("CoordPolar", {
  expect_snapshot({
    construct(ggplot2::coord_polar(theta = "y"))
    construct(ggplot2::coord_polar())
    construct(ggplot2::coord_polar("y", start = pi / 3))
  })
})

test_that("CoordSf", {
  skip_if_not_installed("prettycode", "1.1.0.9000")
  expect_snapshot({
    construct(ggplot2::coord_sf(default_crs = sf::st_crs(4326)))
  })
})

test_that("CoordCartesian", {
  expect_snapshot({
    construct(ggplot2::coord_trans(x = "log10", y = "log10"), check = FALSE)
    construct(ggplot2::coord_trans(x = scales::exp_trans(10), y = scales::exp_trans(10)), check = FALSE)
  })
})

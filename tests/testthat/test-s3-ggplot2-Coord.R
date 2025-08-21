test_that("CoordCartesian", {
  expect_faithful_ggplot_construction(ggplot2::ggplot() + ggplot2::scale_x_continuous(limits = c(325, 500)))
  expect_faithful_ggplot_construction(ggplot2::ggplot() + ggplot2::coord_cartesian(xlim = c(325, 500)))
  expect_faithful_ggplot_construction(ggplot2::ggplot() + ggplot2::coord_cartesian(xlim = c(325, 500)))
  expect_faithful_ggplot_construction(ggplot2::ggplot() + ggplot2::coord_cartesian(xlim = c(325, 500), expand = FALSE))
  expect_faithful_ggplot_construction(ggplot2::ggplot() + ggplot2::coord_cartesian(expand = FALSE))
})

test_that("CoordFixed", {
  expect_snapshot({
    construct(ggplot2::coord_fixed(ratio = 1))
    construct(ggplot2::coord_fixed(ratio = 5))
    construct(ggplot2::coord_fixed(ratio = 1/5))
  })
})

test_that("CoordFixed <= v3.5.2", {
  skip_if_not_installed("ggplot2")
  skip_if(with_versions(ggplot2 > "3.5.2"))
  expect_construct( ggplot2::coord_fixed(xlim = c(15, 30)))
})

test_that("CoordFixed > v3.5.2", {
  # newer version doesn't have a CoordFixed class and this falls back to a regular use of `coord_cartesian()`
  skip_if_not_installed("ggplot2")
  skip_if(with_versions(ggplot2 <= "3.5.2"))
  expect_construct(
    ggplot2::coord_fixed(xlim = c(15, 30)),
    ggplot2::coord_cartesian(xlim = c(15, 30), ratio = 1)
  )
})

test_that("CoordFlip", {
  expect_snapshot({
    construct(ggplot2::coord_flip())
  })
})

test_that("CoordMap", {
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
  skip_if(is.na(suppressWarnings(sf::st_crs(4326)$input)))
  expect_snapshot({
    construct(ggplot2::coord_sf(default_crs = sf::st_crs(4326)), data = list(crs = sf::st_crs(4326)))
  })
})

test_that("CoordTrans <= v3.5.2", {
  skip_if_not_installed("ggplot2")
  skip_if(with_versions(ggplot2 > "3.5.2"))
  expect_silent({
    construct(ggplot2::coord_trans(x = "log10", y = "log10"), check = FALSE)
    construct(ggplot2::coord_trans(x = scales::exp_trans(10), y = scales::exp_trans(10)), check = FALSE)
  })
})

test_that("CoordTransform > v3.5.2", {
  # newer version doesn't uses CoordTransform and deprecates `coord_trans()`
  # in favor or `coord_transform()`
  skip_if_not_installed("ggplot2")
  skip_if(with_versions(ggplot2 <= "3.5.2"))
  expect_snapshot({
    construct(ggplot2::coord_transform(x = "log10", y = "log10"), check = FALSE)
    construct(ggplot2::coord_transform(x = scales::exp_trans(10), y = scales::exp_trans(10)), check = FALSE)
  })
})

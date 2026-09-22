test_that("mixed_units", {
  # unit parsing and deparsing changed in units 1.0
  skip_if_not_installed("units", "1.0-1")
  expect_snapshot({
    construct(units::mixed_units(c(1, 2), c("m", "s")))
    construct(units::mixed_units(c(a = 1, b = NA), c("km/h", "1")))
    construct(units::mixed_units(numeric(), character()))
    construct(structure(units::mixed_units(1, "m"), foo = "bar"))
    construct(units::mixed_units(c(1, 2), c("m", "s")), opts_mixed_units("next"))
  })
})

test_that("mixed_units", {
  skip_if_not_installed("units")
  expect_snapshot({
    construct(units::mixed_units(c(1, 2), c("m", "s")))
    construct(units::mixed_units(numeric(), character()))
    construct(structure(units::mixed_units(1, "m"), foo = "bar"))
    construct(units::mixed_units(c(1, 2), c("m", "s")), opts_mixed_units("next"))
  })
})

# units 1.0 changed the deparsing of unitless objects, deparsed as "1" rather
# than ""
test_that("mixed_units, units < 1.0-1", {
  skip_if_not_installed("units")
  skip_if(with_versions(units >= "1.0-1"))
  expect_snapshot({
    construct(units::mixed_units(c(a = 1, b = NA), c("km/h", "1")))
  })
})

test_that("mixed_units, units >= 1.0-1", {
  skip_if_not_installed("units")
  skip_if(with_versions(units < "1.0-1"))
  expect_snapshot({
    construct(units::mixed_units(c(a = 1, b = NA), c("km/h", "1")))
  })
})

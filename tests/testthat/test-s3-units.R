test_that("units", {
  skip_if_not_installed("units")
  expect_snapshot({
    construct(units::set_units(c(a = 1, b = 2.5), "km/h", mode = "standard"))
    construct(units::set_units(c(1, NA), "kg*m/s^2", mode = "standard"))
    construct(units::set_units(1, "mg/L", mode = "standard"))
    construct(units::set_units(1, "s-1", mode = "standard"))
    construct(units::set_units(numeric(), "m", mode = "standard"))
    construct(units::set_units(matrix(1:4, 2), "m", mode = "standard"))
    construct(units::set_units(array(1:8, c(2, 2, 2)), "m", mode = "standard"))
    construct(units::set_units(structure(1, foo = "bar"), "m", mode = "standard"))
    construct(structure(units::set_units(1, "m", mode = "standard"), class = c("foo", "units")))
    construct(units::set_units(1, "m", mode = "standard"), opts_units("as_units"))
    construct(units::set_units(1, "m", mode = "standard"), opts_units("next"))
  })
})

# units 1.0 changed the parsing and deparsing of units: unitless objects are
# deparsed as "1" rather than "", and `set_units(1, "1/s")` has a "1" numerator,
# which `deparse_unit()` can't reproduce
test_that("units, units < 1.0-1", {
  skip_if_not_installed("units")
  skip_if(with_versions(units >= "1.0-1"))
  expect_snapshot({
    construct(units::set_units(1, 1))
    construct(units::set_units(1, "1/s", mode = "standard"))
    # units attribute in an order `deparse_unit()` doesn't reproduce
    construct(structure(
      1,
      units = structure(list(numerator = c("m", "kg"), denominator = character(0)), class = "symbolic_units"),
      class = "units"
    ))
  })
})

test_that("units, units >= 1.0-1", {
  skip_if_not_installed("units")
  skip_if(with_versions(units < "1.0-1"))
  expect_snapshot({
    construct(units::set_units(1, 1))
    construct(units::set_units(1, "1/s", mode = "standard"))
    construct(structure(
      1,
      units = structure(list(numerator = c("m", "kg"), denominator = character(0)), class = "symbolic_units"),
      class = "units"
    ))
  })
})

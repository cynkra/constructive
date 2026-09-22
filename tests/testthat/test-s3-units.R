test_that("units", {
  # unit parsing and deparsing changed in units 1.0
  skip_if_not_installed("units", "1.0-1")
  expect_snapshot({
    construct(units::set_units(c(a = 1, b = 2.5), "km/h", mode = "standard"))
    construct(units::set_units(c(1, NA), "kg*m/s^2", mode = "standard"))
    construct(units::set_units(1, "mg/L", mode = "standard"))
    construct(units::set_units(1, "s-1", mode = "standard"))
    construct(units::set_units(1, 1))
    construct(units::set_units(numeric(), "m", mode = "standard"))
    construct(units::set_units(matrix(1:4, 2), "m", mode = "standard"))
    construct(units::set_units(array(1:8, c(2, 2, 2)), "m", mode = "standard"))
    construct(units::set_units(structure(1, foo = "bar"), "m", mode = "standard"))
    construct(structure(units::set_units(1, "m", mode = "standard"), class = c("foo", "units")))
    construct(units::set_units(1, "m", mode = "standard"), opts_units("as_units"))
    construct(units::set_units(1, "m", mode = "standard"), opts_units("next"))
    # the numerator is "1", which can't be reproduced from `deparse_unit()`
    construct(units::set_units(1, "1/s", mode = "standard"))
  })
})

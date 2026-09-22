# units

    Code
      construct(units::set_units(c(a = 1, b = 2.5), "km/h", mode = "standard"))
    Output
      units::set_units(c(a = 1, b = 2.5), "km h-1", mode = "standard")
    Code
      construct(units::set_units(c(1, NA), "kg*m/s^2", mode = "standard"))
    Output
      units::set_units(c(1, NA), "kg m s-2", mode = "standard")
    Code
      construct(units::set_units(1, "mg/L", mode = "standard"))
    Output
      units::set_units(1, "mg L-1", mode = "standard")
    Code
      construct(units::set_units(1, "s-1", mode = "standard"))
    Output
      units::set_units(1, "s-1", mode = "standard")
    Code
      construct(units::set_units(1, 1))
    Output
      units::set_units(1, "1", mode = "standard")
    Code
      construct(units::set_units(numeric(), "m", mode = "standard"))
    Output
      units::set_units(numeric(0), "m", mode = "standard")
    Code
      construct(units::set_units(matrix(1:4, 2), "m", mode = "standard"))
    Output
      units::set_units(matrix(seq(1, 4, by = 1), nrow = 2L, ncol = 2L), "m", mode = "standard")
    Code
      construct(units::set_units(array(1:8, c(2, 2, 2)), "m", mode = "standard"))
    Output
      units::set_units(array(seq(1, 8, by = 1), dim = rep(2L, 3L)), "m", mode = "standard")
    Code
      construct(units::set_units(structure(1, foo = "bar"), "m", mode = "standard"))
    Output
      units::set_units(
        1 |>
          structure(foo = "bar"),
        "m",
        mode = "standard"
      )
    Code
      construct(structure(units::set_units(1, "m", mode = "standard"), class = c(
        "foo", "units")))
    Output
      units::set_units(1, "m", mode = "standard") |>
        structure(class = c("foo", "units"))
    Code
      construct(units::set_units(1, "m", mode = "standard"), opts_units("as_units"))
    Output
      units::as_units(1, "m")
    Code
      construct(units::set_units(1, "m", mode = "standard"), opts_units("next"))
    Output
      1 |>
        structure(
          units = list(numerator = "m", denominator = character(0)) |>
            structure(class = "symbolic_units"),
          class = "units"
        )
    Code
      construct(units::set_units(1, "1/s", mode = "standard"))
    Output
      1 |>
        structure(
          units = list(numerator = "1", denominator = "s") |>
            structure(class = "symbolic_units"),
          class = "units"
        )


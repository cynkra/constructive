# mixed_units

    Code
      construct(units::mixed_units(c(1, 2), c("m", "s")))
    Output
      units::mixed_units(c(1, 2), c("m", "s"))
    Code
      construct(units::mixed_units(c(a = 1, b = NA), c("km/h", "1")))
    Output
      units::mixed_units(c(a = 1, b = NA), c("km h-1", "1"))
    Code
      construct(units::mixed_units(numeric(), character()))
    Output
      units::mixed_units(numeric(0), character(0))
    Code
      construct(structure(units::mixed_units(1, "m"), foo = "bar"))
    Output
      units::mixed_units(1, "m") |>
        structure(foo = "bar")
    Code
      construct(units::mixed_units(c(1, 2), c("m", "s")), opts_mixed_units("next"))
    Output
      list(
        units::set_units(1, "m", mode = "standard"),
        units::set_units(2, "s", mode = "standard")
      ) |>
        structure(class = c("mixed_units", "list"))


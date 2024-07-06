# simpleUnit

    Code
      construct(grid::unit(c(1L, 1L, 1L), "cm"))
    Output
      grid::unit(rep(1, 3L), units = "cm")
    Code
      construct_base(grid::unit(c(1L, 1L, 1L), "cm"))
    Output
      grid::unit(rep(1, 3L), units = "cm")
    Code
      construct_dput(grid::unit(c(1L, 1L, 1L), "cm"))
    Output
      rep(1, 3L) |>
        structure(unit = 1L, class = c("simpleUnit", "unit", "unit_v2"))


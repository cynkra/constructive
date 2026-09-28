# vctrs_unspecified

    Code
      construct(vctrs::unspecified(3))
    Output
      vctrs::unspecified(3)
    Code
      construct(vctrs::unspecified(3), opts_vctrs_unspecified("next"))
    Output
      rep(NA, 3L) |>
        structure(class = "vctrs_unspecified")
    Code
      construct(vctrs::unspecified())
    Output
      vctrs::unspecified(0)
    Code
      construct(structure(c(a = NA, b = NA), class = "vctrs_unspecified"))
    Output
      vctrs::unspecified(2) |>
        structure(names = c("a", "b"))
    Code
      construct(structure(c(NA, TRUE), class = "vctrs_unspecified"))
    Output
      c(NA, TRUE) |>
        structure(class = "vctrs_unspecified")


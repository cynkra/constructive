# vctrs_vctr

    Code
      construct(vctrs::new_vctr(c(a = 1, b = NA), unit = "cm", class = "my_vctr"))
    Output
      vctrs::new_vctr(c(a = 1, b = NA), unit = "cm", class = "my_vctr")
    Code
      construct(vctrs::new_vctr(c(a = 1, b = NA), unit = "cm", class = "my_vctr"),
      opts_vctrs_vctr("next"))
    Output
      c(a = 1, b = NA) |>
        structure(unit = "cm", class = c("my_vctr", "vctrs_vctr"))
    Code
      construct(vctrs::new_vctr(character()))
    Output
      vctrs::new_vctr(character(0))
    Code
      construct(vctrs::new_vctr(1:3, inherit_base_type = TRUE))
    Output
      vctrs::new_vctr(1:3, inherit_base_type = TRUE)
    Code
      construct(vctrs::new_vctr(list(1, "a")))
    Output
      vctrs::new_vctr(list(1, "a"))
    Code
      construct(structure(vctrs::new_vctr(1:3), .d = 1, inherit_base_type = 2))
    Output
      vctrs::new_vctr(1:3) |>
        structure(.d = 1, inherit_base_type = 2)
    Code
      construct(structure(list(1, 2), class = "vctrs_vctr"))
    Output
      list(1, 2) |>
        structure(class = "vctrs_vctr")
    Code
      construct(vctrs::list_of(1, 2), opts_vctrs_list_of("next"))
    Output
      vctrs::new_vctr(list(1, 2), ptype = numeric(0), class = "vctrs_list_of")


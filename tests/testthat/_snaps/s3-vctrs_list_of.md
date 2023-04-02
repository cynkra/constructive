# vctrs_list_of

    Code
      construct(vctrs::vec_c(vctrs::list_of(1, 2), vctrs::list_of(FALSE, TRUE)))
    Output
      vctrs::list_of(1, 2, 0, 1, .ptype = numeric(0))
    Code
      construct(vctrs::vec_c(vctrs::list_of(1, 2), vctrs::list_of(FALSE, TRUE)),
      opts_vctrs_list_of("list"))
    Output
      list(1, 2, 0, 1) |>
        structure(ptype = numeric(0), class = c("vctrs_list_of", "vctrs_vctr", "list"))


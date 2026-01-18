# quosure

    Code
      construct(rlang::new_quosure(quote(x), .GlobalEnv))
    Output
      rlang::new_quosure(quote(x), .GlobalEnv)
    Code
      construct(rlang::new_quosure(quote(x), .GlobalEnv), opts_quosure("next"))
    Output
      (~x) |>
        structure(.Environment = .GlobalEnv, class = c("quosure", "formula"))
    Code
      construct(rlang::new_quosure(quote(x), .GlobalEnv), opts_quosure("language"))
    Output
      quote(~x) |>
        structure(.Environment = .GlobalEnv, class = c("quosure", "formula"))


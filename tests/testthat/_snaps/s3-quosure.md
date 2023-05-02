# quosure

    Code
      construct(rlang::new_quosure(quote(x), .GlobalEnv))
    Output
      rlang::new_quosure(quote(x), .GlobalEnv)
    Code
      construct(rlang::new_quosure(quote(x), .GlobalEnv), opts_quosure("next"))
    Output
      (~x) |>
        structure(class = c("quosure", "formula"), .Environment = .GlobalEnv)
    Code
      construct(rlang::new_quosure(quote(x), .GlobalEnv), opts_quosure("language"))
    Output
      quote(~x) |>
        structure(class = c("quosure", "formula"), .Environment = .GlobalEnv)


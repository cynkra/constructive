# quosure

    Code
      construct(rlang::new_quosure(quote(x), .GlobalEnv))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      rlang::new_quosure(quote("x"), .GlobalEnv)
    Code
      construct(rlang::new_quosure(quote(x), .GlobalEnv), opts_quosure("next"))
    Output
      (~x) |>
        structure("class" = c("quosure", "formula"), ".Environment" = .GlobalEnv)
    Code
      construct(rlang::new_quosure(quote(x), .GlobalEnv), opts_quosure("language"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      quote(`"~"`("x")) |>
        structure("class" = c("quosure", "formula"), ".Environment" = .GlobalEnv)


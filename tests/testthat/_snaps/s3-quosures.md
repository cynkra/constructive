# quosures

    Code
      construct(rlang::new_quosures(list(rlang::new_quosure(quote(x)), rlang::new_quosure(
        quote(x), asNamespace("constructive")))))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      rlang::new_quosures(
        list(
          rlang::new_quosure(quote("x")),
          rlang::new_quosure(quote("x"), asNamespace("constructive"))
        )
      )
    Code
      construct(rlang::new_quosures(list(rlang::new_quosure(quote(x)), rlang::new_quosure(
        quote(x), asNamespace("constructive")))), opts_quosures("next"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      list(
        rlang::new_quosure(quote("x")),
        rlang::new_quosure(quote("x"), asNamespace("constructive"))
      ) |>
        structure("class" = c("quosures", "list"), "names" = c("", ""))
    Code
      construct(rlang::new_quosures(list(a = rlang::new_quosure(quote(x)), rlang::new_quosure(
        quote(x), asNamespace("constructive")))), opts_environment("list2env"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      rlang::new_quosures(
        list(
          "a" = rlang::new_quosure(quote("x")),
          rlang::new_quosure(quote("x"), asNamespace("constructive"))
        )
      )


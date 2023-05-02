# quosures

    Code
      construct(rlang::new_quosures(list(rlang::new_quosure(quote(x)), rlang::new_quosure(
        quote(x), asNamespace("constructive")))))
    Output
      rlang::new_quosures(
        list(
          rlang::new_quosure(quote(x)),
          rlang::new_quosure(quote(x), asNamespace("constructive"))
        )
      )
    Code
      construct(rlang::new_quosures(list(rlang::new_quosure(quote(x)), rlang::new_quosure(
        quote(x), asNamespace("constructive")))), opts_quosures("next"))
    Output
      list(
        rlang::new_quosure(quote(x)),
        rlang::new_quosure(quote(x), asNamespace("constructive"))
      ) |>
        structure(class = c("quosures", "list"), names = c("", ""))
    Code
      construct(rlang::new_quosures(list(a = rlang::new_quosure(quote(x)), rlang::new_quosure(
        quote(x), asNamespace("constructive")))), opts_environment("list2env"))
    Output
      rlang::new_quosures(
        list(
          a = rlang::new_quosure(quote(x)),
          rlang::new_quosure(quote(x), asNamespace("constructive"))
        )
      )


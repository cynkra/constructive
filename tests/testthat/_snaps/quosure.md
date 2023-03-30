# quosure

    Code
      construct(rlang::new_quosure(quote(x)), opts_environment("list2env"))
    Output
      rlang::as_quosure(~x, new.env(parent = asNamespace("constructive")))


# quosures

    Code
      construct(rlang::new_quosures(list(rlang::new_quosure(quote(x)), rlang::new_quosure(
        quote(x), new.env()))))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      rlang::as_quosures(
        list(
          rlang::as_quosure(~x, new.env(parent = asNamespace("constructive"))),
          rlang::as_quosure(~x, new.env(parent = asNamespace("constructive")))
        )
      )
    Code
      construct(rlang::new_quosures(list(a = rlang::new_quosure(quote(x)), rlang::new_quosure(
        quote(x), new.env()))))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      rlang::as_quosures(
        list(
          a = rlang::as_quosure(~x, new.env(parent = asNamespace("constructive"))),
          rlang::as_quosure(~x, new.env(parent = asNamespace("constructive")))
        )
      )


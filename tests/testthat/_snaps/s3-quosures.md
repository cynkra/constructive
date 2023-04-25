# quosures

    Code
      construct(rlang::new_quosures(list(rlang::new_quosure(quote(x)), rlang::new_quosure(
        quote(x), asNamespace("constructive")))))
    Condition
      Warning in `.Call()`:
      converting NULL pointer to R NULL
    Message
      ! The code built by {constructive} could not be evaluated.
    Output
      rlang::new_quosures(
        list(
          rlang::new_quosure(
            quote(x),
            constructive::env(
              "0x000000000",
              parents = c("0x000000000", "0x000000000", "namespace:constructive")
            )
          ),
          rlang::new_quosure(quote(x), asNamespace("constructive"))
        )
      )
    Code
      construct(rlang::new_quosures(list(rlang::new_quosure(quote(x)), rlang::new_quosure(
        quote(x), asNamespace("constructive")))), opts_quosures("next"))
    Condition
      Warning in `.Call()`:
      converting NULL pointer to R NULL
    Message
      ! The code built by {constructive} could not be evaluated.
    Output
      list(
        rlang::new_quosure(
          quote(x),
          constructive::env(
            "0x000000000",
            parents = c("0x000000000", "0x000000000", "namespace:constructive")
          )
        ),
        rlang::new_quosure(quote(x), asNamespace("constructive"))
      ) |>
        structure(class = c("quosures", "list"), names = c("", ""))
    Code
      construct(rlang::new_quosures(list(a = rlang::new_quosure(quote(x)), rlang::new_quosure(
        quote(x), asNamespace("constructive")))), opts_environment("list2env"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      rlang::new_quosures(
        list(
          a = rlang::new_quosure(quote(x), new.env(parent = asNamespace("constructive"))),
          rlang::new_quosure(quote(x), asNamespace("constructive"))
        )
      )


# formula

    Code
      fml1 <- formula(lhs ~ rhs, .GlobalEnv)
      construct(fml1)
    Output
      lhs ~ rhs
    Code
      construct(fml1, opts_formula(constructor = "formula"))
    Output
      formula(lhs ~ rhs)
    Code
      construct(fml1, opts_formula(constructor = "new_formula"))
    Output
      rlang::new_formula(quote(lhs), quote(rhs))
    Code
      construct(fml1, opts_formula(environment = TRUE))
    Output
      lhs ~ rhs
    Code
      construct(fml1, opts_formula(constructor = "formula", environment = FALSE))
    Output
      formula(lhs ~ rhs)
    Code
      construct(fml1, opts_formula(constructor = "new_formula", environment = FALSE))
    Output
      rlang::new_formula(quote(lhs), quote(rhs))
    Code
      fml2 <- formula(~rhs, .GlobalEnv)
      construct(fml2)
    Output
      ~rhs
    Code
      construct(fml2, opts_formula(constructor = "formula"))
    Output
      formula(~rhs)
    Code
      construct(fml2, opts_formula(constructor = "new_formula"))
    Output
      rlang::new_formula(NULL, quote(rhs))
    Code
      construct(fml2, opts_formula(environment = TRUE))
    Output
      ~rhs
    Code
      construct(fml2, opts_formula(constructor = "formula", environment = FALSE))
    Output
      formula(~rhs)
    Code
      construct(fml2, opts_formula(constructor = "new_formula", environment = FALSE))
    Output
      rlang::new_formula(NULL, quote(rhs))
    Code
      fml3 <- fml1
      attr(fml3, "foo") <- "bar"
      construct(fml3)
    Output
      (lhs ~ rhs) |>
        structure(foo = "bar")
    Code
      construct(fml3, opts_formula(environment = TRUE))
    Output
      (lhs ~ rhs) |>
        structure(foo = "bar")
    Code
      fml4 <- fml1
      class(fml4) <- "foo"
      construct(fml4)
    Output
      quote(lhs ~ rhs) |>
        structure(class = "foo", .Environment = .GlobalEnv)
    Code
      fml5 <- fml1
      class(fml5) <- NULL
      construct(fml4)
    Output
      quote(lhs ~ rhs) |>
        structure(class = "foo", .Environment = .GlobalEnv)
    Code
      fml6 <- quote(lhs ~ rhs)
      construct(quote(lhs ~ rhs))
    Output
      quote(lhs ~ rhs)


# formula

    Code
      fml1 <- formula(lhs ~ rhs, .GlobalEnv)
      construct(fml1)
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      lhs ~ rhs
    Code
      construct(fml1, opts_formula(constructor = "formula"))
      construct(fml1, opts_formula(constructor = "new_formula"))
      construct(fml1, opts_formula(environment = TRUE))
      construct(fml1, opts_formula(constructor = "formula", environment = FALSE))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      formula(lhs ~ rhs)
    Code
      construct(fml1, opts_formula(constructor = "new_formula", environment = FALSE))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      rlang::new_formula(quote(lhs), quote(rhs))
    Code
      fml2 <- formula(~rhs, .GlobalEnv)
      construct(fml2)
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      ~rhs
    Code
      construct(fml2, opts_formula(constructor = "formula"))
      construct(fml2, opts_formula(constructor = "new_formula"))
      construct(fml2, opts_formula(environment = TRUE))
      construct(fml2, opts_formula(constructor = "formula", environment = FALSE))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      formula(~rhs)
    Code
      construct(fml2, opts_formula(constructor = "new_formula", environment = FALSE))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      rlang::new_formula(NULL, quote(rhs))
    Code
      fml3 <- fml1
      attr(fml3, "foo") <- "bar"
      construct(fml3)
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      (lhs ~ rhs) |>
        structure(foo = "bar")
    Code
      construct(fml3, opts_formula(environment = TRUE))
      fml4 <- fml1
      class(fml4) <- "foo"
      construct(fml4)
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      (lhs ~ rhs) |>
        structure(class = "foo")
    Code
      fml5 <- fml1
      class(fml5) <- NULL
      construct(fml4)
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      (lhs ~ rhs) |>
        structure(class = "foo")


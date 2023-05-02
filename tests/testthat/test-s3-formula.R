test_that("formula", {
  #testthat::skip("skipping formula tests")
  local(
    envir = .GlobalEnv,
    expect_snapshot({
      fml1 <- formula(lhs ~ rhs, .GlobalEnv)
      construct(fml1) # by default no env for constructor = "~"
      construct(fml1, opts_formula(constructor = "formula"))
      construct(fml1, opts_formula(constructor = "new_formula"))
      construct(fml1, opts_formula(environment = TRUE))
      construct(fml1, opts_formula(constructor = "formula", environment = FALSE))
      construct(fml1, opts_formula(constructor = "new_formula", environment = FALSE))

      fml2 <- formula(~ rhs, .GlobalEnv)
      construct(fml2) # by default no env for constructor = "~"
      construct(fml2, opts_formula(constructor = "formula"))
      construct(fml2, opts_formula(constructor = "new_formula"))
      construct(fml2, opts_formula(environment = TRUE))
      construct(fml2, opts_formula(constructor = "formula", environment = FALSE))
      construct(fml2, opts_formula(constructor = "new_formula", environment = FALSE))

      fml3 <- fml1
      attr(fml3, "foo") <- "bar"
      construct(fml3)
      construct(fml3, opts_formula(environment = TRUE))

      # fml4, fml5, fml6 fall back to the language method, because `~` adds
      # a formula class to the object
      fml4 <- fml1
      class(fml4) <- "foo"
      construct(fml4)

      fml5 <- fml1
      class(fml5) <- NULL
      construct(fml4)

      fml6 <- quote(lhs ~ rhs)
      construct(quote(lhs ~ rhs))
    })
  )
})

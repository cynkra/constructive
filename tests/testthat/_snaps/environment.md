# environment

    Code
      construct(globalenv())
    Output
      .GlobalEnv
    Code
      construct(baseenv())
    Output
      baseenv()
    Code
      construct(as.environment("package:base"))
    Output
      baseenv()
    Code
      construct(asNamespace("base"))
    Output
      .BaseNamespaceEnv
    Code
      construct(as.environment("Autoloads"))
    Output
      as.environment("Autoloads")
    Code
      construct(environment(setNames))
    Output
      asNamespace("stats")
    Code
      construct(as.environment(head(cars, 2)))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      list2env(list(dist = c(2, 10), speed = c(4, 4)), parent = .GlobalEnv)
    Code
      construct(as.environment(head(cars, 2)), opts_environment(constructor = "new.env"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      new.env()
    Code
      construct(environment(setNames), opts_environment(constructor = "new.env"))
    Output
      asNamespace("stats")
    Code
      env <- new.env()
      class(env) <- "foo"
      construct(env)
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      new.env(parent = asNamespace("constructive")) |>
        structure(class = "foo")
    Code
      e1 <- new.env(parent = .GlobalEnv)
      e1$x <- 1
      e2 <- new.env(parent = e1)
      e2$y <- 2
      construct(e2)
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      list2env(list(y = 2), parent = .GlobalEnv)
    Code
      construct(e2, opts_environment(recurse = TRUE))
    Output
      .GlobalEnv |>
        list2env(list(x = 1), parent = _) |>
        list2env(list(y = 2), parent = _)
    Code
      construct(e2, opts_environment(constructor = "new_environment"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      rlang::new_environment(list(y = 2), parent = .GlobalEnv)
    Code
      construct(e2, opts_environment(constructor = "new_environment", recurse = TRUE))
    Output
      .GlobalEnv |>
        rlang::new_environment(list(x = 1), parent = _) |>
        rlang::new_environment(list(y = 2), parent = _)
    Code
      construct(e2, opts_environment(constructor = "new.env"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      new.env()
    Code
      construct(e2, opts_environment(constructor = "topenv"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      .GlobalEnv
    Code
      construct(e2, opts_environment(constructor = "as.environment"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      as.environment(list(y = 2))


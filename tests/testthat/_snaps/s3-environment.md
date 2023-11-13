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
      construct(as.environment(head(cars, 2)), opts_environment("list2env"))
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
      env <- new.env(parent = asNamespace("stats"))
      class(env) <- "foo"
      construct(env, opts_environment("list2env"))
    Output
      new.env(parent = asNamespace("stats")) |>
        structure(class = "foo")
    Code
      e1 <- new.env(parent = .GlobalEnv)
      e1$x <- 1
      e2 <- new.env(parent = e1)
      e2$y <- 2
      e2$.z <- 3
      construct(e2, opts_environment(constructor = "list2env"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      list2env(list(.z = 3, y = 2), parent = .GlobalEnv)
    Code
      construct(e2, opts_environment(constructor = "new_environment"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      rlang::new_environment(list(.z = 3, y = 2), parent = .GlobalEnv)
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
      as.environment(list(.z = 3, y = 2))
    Code
      evalq({
        e <- new.env()
        e$f <- e
        foo <- evalq(~a, e)
        construct(foo, opts_environment(predefine = TRUE), opts_formula(environment = TRUE))
      }, .GlobalEnv)
    Output
      ..env.1.. <- new.env(parent = .GlobalEnv)
      ..env.1..$f <- ..env.1..
      (~a) |>
        structure(.Environment = ..env.1..)

---

    Code
      construct(e2, opts_environment(constructor = "list2env", recurse = TRUE))
    Output
      .GlobalEnv |>
        list2env(list(x = 1), parent = _) |>
        list2env(list(.z = 3, y = 2), parent = _)
    Code
      construct(e2, opts_environment(constructor = "new_environment", recurse = TRUE))
    Output
      .GlobalEnv |>
        rlang::new_environment(list(x = 1), parent = _) |>
        rlang::new_environment(list(.z = 3, y = 2), parent = _)

---

    Code
      construct(constructive::.cstr_construct, opts_environment(predefine = TRUE),
      opts_function(environment = TRUE))
    Output
      (function(x, ..., data = NULL) {
        data_name <- perfect_match(x, data)
        if (!is.null(data_name)) return(data_name)
        UseMethod(".cstr_construct")
      }) |>
        (`environment<-`)(asNamespace("constructive"))


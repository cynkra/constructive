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
      construct(constructive::construct, opts_environment(predefine = TRUE),
      opts_function(environment = TRUE))
    Output
      (function(x, ..., data = NULL, pipe = NULL, check = NULL,
                            compare = compare_options(), one_liner = FALSE,
                            template = getOption("constructive_opts_template")) {
      
        # reset globals
        globals$predefinition <- character()
        globals$envs <- data.frame(hash = character(), name = character())
      
        # check inputs
        .cstr_combine_errors(
          # force so we might fail outside of the try_fetch() when x is not properly provided
          force(x),
          ellipsis::check_dots_unnamed(),
          abort_wrong_data(data),
          abort_not_boolean(one_liner)
        )
      
        # process data into a flat named list of objects
        data <- process_data(data)
      
        # build code that produces the object, prepend with predefinitions if relevant
        caller <- caller_env()
        code <- try_construct(x, template = template, ..., data = data, pipe = pipe, one_liner = one_liner, env = caller)
        code <- c(globals$predefinition, code)
      
        # for https://github.com/cynkra/constructive/issues/101
        Encoding(code) <- "UTF-8"
      
        # attempt to parse, and style if successful
        styled_code <- try_parse(code, one_liner)
      
        # check output fidelity if relevant, signal issues and update globals$issues
        compare <- check_round_trip(x, styled_code, data, check, compare, caller)
      
        # build a new constructive object, leave the display work to the print method
        new_constructive(styled_code, compare)
      }) |>
        (`environment<-`)(asNamespace("constructive"))


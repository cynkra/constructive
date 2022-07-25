# formula

    Code
      evalq(construct(~a), .GlobalEnv)
    Output
      rlang::set_env(~a, .GlobalEnv)
    Code
      construct(local(~a), check = FALSE)
    Output
      rlang::set_env(~a, as.environment(list()))
    Code
      construct(local(~a), check = FALSE, env_as_list = FALSE)
    Output
      rlang::set_env(~a, new.env())
    Code
      x <- ~a
      class(x) <- "foo"
      construct(x, check = FALSE, env_as_list = FALSE)
    Output
      rlang::set_env(~a, new.env()) |>
        structure(class = "foo")
    Code
      y <- ~classless
      class(y) <- NULL
      construct(y, check = FALSE, env_as_list = FALSE)
    Output
      rlang::set_env(~classless, new.env()) |>
        structure(class = NULL)


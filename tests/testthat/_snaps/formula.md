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


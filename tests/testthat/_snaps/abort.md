# abort

    Code
      foo <- (function(x = c("a", "b"), y, z, ...) {
        .cstr_combine_errors(x <- rlang::arg_match(x), abort_not_boolean(y),
        abort_not_null_or_integerish(z), rlang::check_dots_empty())
      })
      err <- try(foo("z", "z", "z", "z"), silent = TRUE)
      cat(attr(err, "condition")$message)
    Output
      `x` must be one of "a" or "b", not "z".
      ! `y` is not a boolean (scalar `TRUE` or `FALSE`)
      i It has type 'character' and length 1:
      "z"
      ! `z` is not `NULL` or a scalar integerish 
      i It has type 'character' and length 1:
      "z"
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = "z"
      i Did you forget to name an argument?
    Code
      try(abort_not_string(mean))
    Output
      Error in eval(code, test_env) : `mean` must be a string.
      i It has type 'closure':
      (function(x, ...) UseMethod("mean")) |>
        (`environment<-`)(.BaseNamespaceEnv)
    Code
      try(abort_not_env_or_named_list(letters))
    Output
      Error in eval(code, test_env) : 
        `letters` must be a named list or an environment.
      i It has type 'character' and length 26:
      c(
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
        "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"
      )
    Code
      try(abort_not_env_or_named_list(list(1, 2)))
    Output
      Error in eval(code, test_env) : 
        `list` must be a named list or an environment.
      `1` must be a named list or an environment.
      `2` must be a named list or an environment.
      i It is a list with unnamed elements.
    Code
      try(abort_wrong_data(letters))
    Output
      Error in eval(code, test_env) : `data` has an unexpected value.
      i It has type 'character' and length 26:
      c(
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
        "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"
      )
    Code
      try(abort_wrong_data(list(letters)))
    Output
      Error in eval(code, test_env) : `data[[1]]` has an unexpected value
      i It has type 'character' and length 26:
      c(
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
        "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"
      )
    Code
      try(abort_wrong_data("unknown"))
    Output
      Error in eval(code, test_env) : 
        `data` can be a string only if it's an installed package name.
      i There is no installed package called 'unknown'
    Code
      try(abort_wrong_data(list("unknown")))
    Output
      Error in eval(code, test_env) : 
        `data` can contain unnamed strings only if it's an installed package name.
      i `data[[1]]` is "unknown".
      i There is no installed package called 'unknown'
    Code
      try(abort_wrong_data(mean))
    Output
      Error in eval(code, test_env) : `data` has an unexpected value.
      i It has type 'closure':
      (function(x, ...) UseMethod("mean")) |>
        (`environment<-`)(.BaseNamespaceEnv)
    Code
      try(abort_wrong_data(list(mean)))
    Output
      Error in eval(code, test_env) : 
        `data[[1]]` is unnamed and has an unexpected value.
      i It has type 'closure':
      (function(x, ...) UseMethod("mean")) |>
        (`environment<-`)(.BaseNamespaceEnv)

# describe

    Code
      writeLines(describe(letters))
    Output
      It has type 'character' and length 26:
      c(
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
        "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"
      )
    Code
      writeLines(describe(mean))
    Output
      It has type 'closure':
      (function(x, ...) UseMethod("mean")) |>
        (`environment<-`)(.BaseNamespaceEnv)


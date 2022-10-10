# abort

    Code
      foo <- (function(x = c("a", "b"), y, z, ...) {
        combine_errors(x <- rlang::arg_match(x), abort_not_boolean(y),
        abort_not_null_or_integerish(z), ellipsis::check_dots_empty())
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


#' construct_reprex
#'
#' @description
#'
#' * `construct_reprex()` creates a reprex that constructs the parent environment
#' and last call on the stack. Y
#' * `setup_reprex()` sets up a function `fun` so it will call `construct_reprex()`
#' with the same additional arguments next time `fun` is called.
#'
#' @details
#'
#' You might use `construct_reprex()` with `options(error = recover)`
#' to create a reprex at the chosen frame in the call stack.
#'
#' Note the `construct_reprex()` doesn't look at the local environment, if you
#' want to reproduce the local environment when debugging you might use `construct_multi(environment())`
#' instead.
#'
#' @param fun A function
#' @param ... Forwarded to `construct_multi()`
#'
#' @return Both functions are called for side-effects and return `NULL` invisibly.
#' @export
construct_reprex <- function(...) {
  caller_env <- parent.frame(2)
  call <- sys.call(-1)
  fun <- sys.function(-1)

  on.exit({
    if (!is.null(attr(fun, "constructive_modified_from"))) {
      fun_chr <- sub("^.*?::(.*)$", "\\1", paste(deparse(call[[1]]), collapse = ""))
      ns <- topenv(environment(fun))
      assignInNamespace(fun_chr, attr(fun, "constructive_modified_from"), ns)
    }
  })

  # output ---------------------------------------------------------------------
  if (length(names(caller_env))) {
    constructed <- construct_multi(caller_env, ...)
    constructed$code <- as_constructive_code(
      c(constructed$code, deparse_call(call, style = FALSE))
    )
  } else {
    constructed <- new_constructive(code = deparse_call(call), compare = NULL)
  }
  constructed
}

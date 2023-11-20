#' construct_reprex
#'
#' @description
#'
#' `construct_reprex()` creates a reprex that constructs the local environment,
#'   or a caller environment `n` steps above. If `n > 0` the function call
#'   is included by default.
#'
#' @details
#'
#' A useful trick is to use `construct_reprex()` with `options(error = recover)`
#' to be able to reproduce an error.
#'
#' @param n The number of steps to go up on the call stack
#' @param include_call Whether to include the call to the function that created the next
#'   frame, ignored for `n == 0`
#' @param ... Forwarded to `construct_multi()`
#'
#' @return Returns return `NULL` invisibly, called for side-effects.
#' @export
construct_reprex <- function(n = 0, include_call = TRUE, ...) {
  stopifnot(n >= 0)
  caller_env <- parent.frame(1 + n)
  if (n == 0) {
    return(construct_multi(caller_env, ...))
  }

  call <- sys.call(-n)
  fun <- sys.function(-n)

  # output ---------------------------------------------------------------------
  if (length(names(caller_env))) {
    constructed <- construct_multi(caller_env, ...)
    if (include_call) {
      constructed$code <- as_constructive_code(
        c(constructed$code, deparse_call(call, style = FALSE))
      )
    }
  } else {
    if (include_call) {
      code <- deparse_call(call)
    } else {
      code <- ""
    }
    constructed <- new_constructive(code, compare = NULL)
  }
  constructed
}

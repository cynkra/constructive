#' construct_reprex
#'
#' @description
#'
#' `construct_reprex()` constructs all objects of the local environment,
#'   or a caller environment `n` steps above. If `n > 0` the function call
#'   is also included in a comment.
#'
#' @details
#'
#' `construct_reprex()` doesn't call the \{reprex\} package but it shares
#'  the purpose of making it easier to reproduce an output, hence the name.
#'  If you want to it to look more like a `reprex::reprex` consider `options(constructive_print_mode = "reprex")`.
#'  See `?constructive_print_mode` for more.
#'
#' `construct_reprex()` wraps `construct_multi()` and is thus able to construct
#' unevaluated arguments using `delayedAssign()`. This means we can construct
#' reprexes for functions that use Non Standard Evaluation.
#'
#' A useful trick is to use `construct_reprex()` with `options(error = recover)`
#' to be able to reproduce an error.
#'
#' `construct_reprex()` might fail to reproduce the output of functions that refer
#' to environments other than their caller environment. We believe these are
#' very rare and that the simplicity is worth the rounded corners, but if you
#' encounter these limitations please do open a ticket on our issue tracker
#' at `https://github.com/cynkra/constructive/` and we might expand the feature.
#'
#' @param n The number of steps to go up on the call stack
#' @param ... Forwarded to `construct_multi()`
#'
#' @return Returns return `NULL` invisibly, called for side-effects.
#' @export
construct_reprex <- function(n = 0, ...) {
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
    constructed$code <- as_constructive_code(
      c(constructed$code, paste("#", deparse_call(call, style = FALSE)))
    )
  } else {
    code <- paste("#", deparse_call(call))
    constructed <- new_constructive(code, compare = NULL)
  }
  constructed
}

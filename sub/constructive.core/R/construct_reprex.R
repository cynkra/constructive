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
#' `construct_reprex()` doesn't call the \{reprex\} package. `construct_reprex()`
#' builds reproducible data while the reprex package build reproducible output
#' once you have the data.
#'
#' `construct_reprex()` wraps `construct_multi()` and is thus able to construct
#' unevaluated arguments using `delayedAssign()`. This means we can construct
#' reprexes for functions that use Non Standard Evaluation.
#'
#' A useful trick is to use `options(error = recover)` to be able to inspect
#' frames on error, and use `construct_reprex()` from there to reproduce the
#' data state.
#'
#' `construct_reprex()` might fail to reproduce the output of functions that refer
#' to environments other than their caller environment. We believe these are
#' very rare and that the simplicity is worth the rounded corners, but if you
#' encounter these limitations please do open a ticket on our issue tracker
#' at `https://github.com/cynkra/constructive/` and we might expand the feature.
#'
#' @param ... Forwarded to `construct_multi()`
#' @param n The number of steps to go up on the call stack
#' @inheritParams construct_multi
#'
#' @return An object of class 'constructive'.
#' @seealso [construct_multi()]
#' @export
construct_reprex <- function(..., n = 0, include_dotted = TRUE) {
  stopifnot(n >= 0)
  caller_env <- parent.frame(1 + n)
  if (n == 0) {
    return(construct_multi(caller_env, ..., include_dotted = include_dotted))
  }

  call <- sys.call(-n)
  fun <- sys.function(-n)

  # output ---------------------------------------------------------------------
  if (length(names(caller_env))) {
    constructed <- construct_multi(caller_env, ..., include_dotted = include_dotted)
    constructed$code <- as_constructive_code(
      c(constructed$code, paste("#", deparse_call(call, style = FALSE)))
    )
  } else {
    code <- paste("#", deparse_call(call))
    constructed <- new_constructive(code, compare = NULL)
  }
  constructed
}

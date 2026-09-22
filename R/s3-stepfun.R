#' Constructive options for class 'stepfun'
#'
#' These options will be used on objects of class 'stepfun'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"stepfun"` (default): We build the object using `stepfun()`, the arguments
#'   are retrieved from the function's enclosure. The "call" attribute is
#'   repaired if it differs from the constructed call.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `opts_function()`.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_stepfun>
#' @export
opts_stepfun <- function(constructor = c("stepfun", "next"), ...) {
  .cstr_options("stepfun", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct stepfun
.cstr_construct.stepfun <- function(x, ...) {
  opts <- list(...)$opts$stepfun %||% opts_stepfun()
  if (is_corrupted_stepfun(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.stepfun", structure(NA, class = opts$constructor))
}

is_corrupted_stepfun <- function(x) {
  is.null(stepfun_args(x))
}

#' @export
#' @method .cstr_construct.stepfun stepfun
.cstr_construct.stepfun.stepfun <- function(x, ...) {
  code <- .cstr_apply(stepfun_args(x), "stepfun", ...)
  repair_attributes_stepfun(x, code, ...)
}

repair_attributes_stepfun <- function(x, code, ...) {
  repair_attributes_approxfun(
    x, code, ...,
    idiomatic_class = c("stepfun", "function")
  )
}

# Retrieve the arguments of the `stepfun()` call from the enclosure of `x`,
# returns `NULL` if `stepfun()` can't reproduce `x`
stepfun_args <- function(x) {
  if (!is_approxfun_like(x, stats::stepfun(1, c(0, 1)))) return(NULL)
  env <- environment(x)
  n <- length(env$x)
  if (!n || typeof(env$yleft) != typeof(env$yright)) return(NULL)
  # stepfun() drops the first value of `y`, or the last one if `right = TRUE`
  right <- !identical(env$y[[n]], as.double(env$yright))
  y <- if (right) c(env$y, env$yright) else c(env$yleft, env$y)
  storage.mode(y) <- typeof(env$yleft)
  y_env <- if (right) y[-(n + 1)] else y[-1]
  if (
    !identical(as.double(y_env), env$y) ||
    !identical(y[[1]], env$yleft) ||
    !identical(y[[n + 1]], env$yright)
  ) {
    return(NULL)
  }
  args <- list(env$x, y)
  if (!identical(env$f, as.numeric(right))) args$f <- env$f
  if (right) args$right <- TRUE
  args
}

# Checks that `x` is a function built by `approxfun()` from the same call
# as `ref`, with the same enclosure variables, and with its class and attributes
# (not checked here) as only possible differences.
is_approxfun_like <- function(x, ref) {
  if (typeof(x) != "closure") return(FALSE)
  if (!identical(formals(x), formals(ref)) || !identical(body(x), body(ref))) return(FALSE)
  env <- environment(x)
  ref_env <- environment(ref)
  if (!identical(parent.env(env), parent.env(ref_env))) return(FALSE)
  nms <- sort(ls(env, all.names = TRUE))
  if (!identical(nms, sort(ls(ref_env, all.names = TRUE)))) return(FALSE)
  if (!identical(env$method, ref_env$method) || !identical(env$na.rm, ref_env$na.rm)) return(FALSE)
  if (!is.double(env$x) || !is.double(env$y) || length(env$x) != length(env$y)) return(FALSE)
  if (anyNA(env$x) || is.unsorted(env$x, strictly = TRUE)) return(FALSE)
  TRUE
}

repair_attributes_approxfun <- function(x, code, ..., idiomatic_class) {
  # `stepfun()` and `ecdf()` store their call, we repair it only if it differs
  # from the constructed call
  call <- attr(x, "call")
  ignore <- if (identical(call, str2lang(paste(code, collapse = "\n")))) "call"
  remove <- if (is.null(call)) "call"
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = idiomatic_class,
    ignore = ignore,
    remove = remove
  )
}

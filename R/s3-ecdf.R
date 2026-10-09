#' Constructive options for class 'ecdf'
#'
#' These options will be used on objects of class 'ecdf'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"ecdf"` (default): We build the object using `ecdf()`, the sorted
#'   non-missing data is retrieved from the function's enclosure. The "call"
#'   attribute is repaired if it differs from the constructed call.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `opts_stepfun()`.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_ecdf>
#' @export
opts_ecdf <- function(constructor = c("ecdf", "next"), ...) {
  .cstr_options("ecdf", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ecdf
.cstr_construct.ecdf <- function(x, ...) {
  opts <- list(...)$opts$ecdf %||% opts_ecdf()
  if (is_corrupted_ecdf(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ecdf", structure(NA, class = opts$constructor))
}

is_corrupted_ecdf <- function(x) {
  is.null(ecdf_data(x))
}

#' @export
#' @method .cstr_construct.ecdf ecdf
.cstr_construct.ecdf.ecdf <- function(x, ...) {
  code <- .cstr_apply(list(ecdf_data(x)), "ecdf", ...)
  repair_attributes_ecdf(x, code, ...)
}

repair_attributes_ecdf <- function(x, code, ...) {
  repair_attributes_approxfun(
    x, code, ...,
    idiomatic_class = c("ecdf", "stepfun", "function")
  )
}

# Retrieve the sorted data given to `ecdf()` from the enclosure of `x`,
# returns `NULL` if `ecdf()` can't reproduce `x`
ecdf_data <- function(x) {
  ref <- stats::ecdf(0)
  if (!is_approxfun_like(x, ref)) return(NULL)
  env <- environment(x)
  ref_env <- environment(ref)
  for (nm in c("yleft", "yright", "f")) {
    if (!identical(env[[nm]], ref_env[[nm]])) return(NULL)
  }
  nobs <- env$nobs
  if (!is.integer(nobs) || length(nobs) != 1 || is.na(nobs) || !length(env$x)) return(NULL)
  times <- round(diff(c(0, env$y)) * nobs)
  if (any(times < 1) || sum(times) != nobs || !identical(cumsum(times) / nobs, env$y)) {
    return(NULL)
  }
  rep(env$x, times)
}

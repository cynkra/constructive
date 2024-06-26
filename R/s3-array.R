#' Constructive options for arrays
#'
#' These options will be used on arrays. Note that arrays can be built on top of
#' vectors, lists or expressions. Canonical arrays have an implicit class "array"
#' shown by `class()` but "array" is not part of the class attribute.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"array"` (default): Use the `array()` function
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_array>
#' @export
opts_array <- function(constructor = c("array", "next"), ...) {
  .cstr_options("array", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct array
.cstr_construct.array <- function(x, ...) {
  opts <- list(...)$opts$array %||% opts_array()
  if (is_corrupted_array(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.array", structure(NA, class = opts$constructor))
}

is_corrupted_array <- function(x) {
  !("dim" %in% names(attributes(x)) && (is.atomic(x) || is.list(x) || is.expression(x)))
}

#' @export
#' @method .cstr_construct.array array
.cstr_construct.array.array <- function(x, ...) {
  # build args for array() call
  x_stripped <- x
  attributes(x_stripped) <- NULL
  args <- list(
    x_stripped,
    dim = attr(x, "dim")
  )
  dimnames <- attr(x, "dimnames")
  args$dim_names <- if (!is.null(dimnames)) list(dimnames = dimnames)

  # build code
  code <- .cstr_apply(args, fun = "array", ...)

  # repair
  repair_attributes_array(x, code, ...)
}

repair_attributes_array <- function(x, code, ..., pipe = NULL) {
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = "dim"
  )
}

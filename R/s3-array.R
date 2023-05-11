constructors$array <- new.env()

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
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_array>
#' @export
opts_array <- function(constructor = c("array", "next"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  constructive_options("array", constructor = constructor)
}

#' @export
construct_raw.array <- function(x, ...) {
  opts <- fetch_opts("array", ...)
  if (is_corrupted_array(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$array[[opts$constructor]]
  constructor(x, ...)
}

#' @export
is_corrupted_array <- function(x) {
  !("dim" %in% names(attributes(x)) && (is.atomic(x) || is.list(x) || is.expression(x)))
}

constructors$array$array <- function(x, ...) {
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
  code <- construct_apply(args, fun = "array", ...)

  # repair
  repair_attributes.array(x, code, ...)
}

#' @export
repair_attributes.array <- function(x, code, ..., pipe ="base") {
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = c("dim")
  )
}

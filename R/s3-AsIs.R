constructors$array <- new.env()

#' Constructive options for the class `AsIs`
#'
#' These options will be used on objects of class `AsIs`. `AsIs` objects are
#' created with `I()` which only prepends `"AsIs"` to the class attribute.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"I"` (default): Use the `I()` function
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"atomic"` : We define as an atomic vector and repair attributes
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_array>
#' @export
opts_AsIs <- function(constructor = c("I", "next", "atomic"), ...) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "AsIs"),
    check_dots_empty()
  )
  .cstr_options("AsIs", constructor = constructor)
}

#' @export
.cstr_construct.AsIs <- function(x, ...) {
  opts <- .cstr_fetch_opts("AsIs", ...)
  if (is_corrupted_AsIs(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$AsIs[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_AsIs <- function(x) {
  oldClass(x)[[1]] != "AsIs"
}

constructors$AsIs$I <- function(x, ...) {
  x_stripped <- x
  cl <- oldClass(x)
  class(x_stripped) <- setdiff(cl, "AsIs")
  # no validation needed
  code <- .cstr_wrap(.cstr_construct(x_stripped, ...), "I", new_line = FALSE)
  repair_attributes_AsIs(x, code, ...)
}

constructors$AsIs$atomic <- function(x, ...) {
  .cstr_construct.atomic(x, ...)
}

repair_attributes_AsIs <- function(x, code, ...) {
  # no reparation needed, this will be dealt with in `I()`'s arg
  code
}

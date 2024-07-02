#' Constructive options for the class `AsIs`
#'
#' These options will be used on objects of class `AsIs`. `AsIs` objects are
#' created with `I()` which only prepends `"AsIs"` to the class attribute.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"I"` (default): Use the `I()` function
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_AsIs>
#' @export
opts_AsIs <- function(constructor = c("I", "next"), ...) {
  .cstr_options("AsIs", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct AsIs
.cstr_construct.AsIs <- function(x, ...) {
  opts <- list(...)$opts$AsIs %||% opts_AsIs()
  if (is_corrupted_AsIs(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.AsIs", structure(NA, class = opts$constructor))
}

is_corrupted_AsIs <- function(x) {
  oldClass(x)[[1]] != "AsIs"
}

#' @export
#' @method .cstr_construct.AsIs I
.cstr_construct.AsIs.I <- function(x, ...) {
  x_stripped <- x
  cl <- oldClass(x)
  class(x_stripped) <- setdiff(cl, "AsIs")
  # no validation needed
  code <- .cstr_wrap(.cstr_construct(x_stripped, ...), "I", new_line = FALSE)
  repair_attributes_AsIs(x, code, ...)
}

repair_attributes_AsIs <- function(x, code, ...) {
  # no repair needed, this will be dealt with in `I()`'s arg
  code
}

#' Constructive options for class 'mixed_units'
#'
#' These options will be used on objects of class 'mixed_units'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"mixed_units"` (default): We build the object using
#'   `units::mixed_units()` on a double vector and a character vector of unit
#'   strings as given by `units::deparse_unit()`.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_mixed_units>
#' @export
opts_mixed_units <- function(constructor = c("mixed_units", "next"), ...) {
  .cstr_options("mixed_units", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct mixed_units
.cstr_construct.mixed_units <- function(x, ...) {
  opts <- list(...)$opts$mixed_units %||% opts_mixed_units()
  if (is_corrupted_mixed_units(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.mixed_units", structure(NA, class = opts$constructor))
}

is_corrupted_mixed_units <- function(x) {
  if (typeof(x) != "list") return(TRUE)
  is_scalar_units <- function(elt) {
    identical(names(attributes(elt)), c("units", "class")) &&
      identical(oldClass(elt), "units") &&
      length(elt) == 1 &&
      !is_corrupted_units(elt)
  }
  !all(vapply(strip(x), is_scalar_units, logical(1)))
}

#' @export
#' @method .cstr_construct.mixed_units mixed_units
.cstr_construct.mixed_units.mixed_units <- function(x, ...) {
  elts <- strip(x)
  values <- vapply(elts, strip, numeric(1))
  units <- vapply(elts, units::deparse_unit, character(1), USE.NAMES = FALSE)
  code <- .cstr_apply(list(values, units), "units::mixed_units", ...)
  repair_attributes_mixed_units(x, code, ...)
}

repair_attributes_mixed_units <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("mixed_units", "list")
  )
}

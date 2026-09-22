#' Constructive options for class 'units'
#'
#' These options will be used on objects of class 'units'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"set_units"` (default): We build the object using
#'   `units::set_units(x, value, mode = "standard")` where `value` is the unit
#'   string as given by `units::deparse_unit()`.
#' * `"as_units"` : We build the object using `units::as_units(x, value)`.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_units>
#' @export
opts_units <- function(constructor = c("set_units", "as_units", "next"), ...) {
  .cstr_options("units", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct units
.cstr_construct.units <- function(x, ...) {
  opts <- list(...)$opts$units %||% opts_units()
  if (is_corrupted_units(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.units", structure(NA, class = opts$constructor))
}

is_corrupted_units <- function(x) {
  units <- attr(x, "units")
  if (
    typeof(x) != "double" ||
    !identical(oldClass(units), "symbolic_units") ||
    !identical(names(units), c("numerator", "denominator")) ||
    !is.character(units$numerator) ||
    !is.character(units$denominator)
  ) {
    return(TRUE)
  }
  # the unit string must reproduce the units attribute exactly
  rebuilt <- tryCatch(
    units::set_units(1, units::deparse_unit(x), mode = "standard"),
    error = function(e) NULL
  )
  !identical(attr(rebuilt, "units"), units)
}

#' @export
#' @method .cstr_construct.units set_units
.cstr_construct.units.set_units <- function(x, ...) {
  args <- list(strip_units(x), units::deparse_unit(x), mode = "standard")
  code <- .cstr_apply(args, "units::set_units", ...)
  repair_attributes_units(x, code, ...)
}

#' @export
#' @method .cstr_construct.units as_units
.cstr_construct.units.as_units <- function(x, ...) {
  args <- list(strip_units(x), units::deparse_unit(x))
  code <- .cstr_apply(args, "units::as_units", ...)
  repair_attributes_units(x, code, ...)
}

# keep dim, names and other attributes so they're built with the underlying
# double vector
strip_units <- function(x) {
  attrs <- attributes(x)
  attrs[c("units", "class")] <- NULL
  attributes(x) <- attrs
  x
}

repair_attributes_units <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "units",
    ignore = setdiff(names(attributes(x)), "class")
  )
}

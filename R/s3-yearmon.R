#' Constructive options for class 'yearmon'
#'
#' These options will be used on objects of class 'yearmon'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as.yearmon"` (default): We build the object using `zoo::as.yearmon()` on
#'   a string in the format `"2000 Q3"`.
#' * `"yearmon"` : We build the object using `zoo::yearmon()` on a double in the
#'   format `2000.5`
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_yearmon>
#' @export
opts_yearmon <- function(constructor = c("as.yearmon", "yearmon", "next"), ...) {
  .cstr_options("yearmon", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct yearmon
.cstr_construct.yearmon <- function(x, ...) {
  opts <- list(...)$opts$yearmon %||% opts_yearmon()
  if (is_corrupted_yearmon(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.yearmon", structure(NA, class = opts$constructor))
}

is_corrupted_yearmon <- function(x) {
  # not x %% (1/12) because of floating point issues
  !is.double(x) || any((12 * x) %% 1 != 0)
}

#' @export
#' @method .cstr_construct.yearmon as.yearmon
.cstr_construct.yearmon.as.yearmon <- function(x, ...) {
  # code from zoo:::as.Date.yearmon()
  year <- floor(unclass(x) + 0.001)
  month <- floor(12 * (unclass(x) - year) + 1 + 0.5 + 0.001)
  date <- as.Date(sprintf("%s-%s-01", year, month))
  args <- list(format(date, format = "%b %Y"))
  code <- .cstr_apply(args, fun = "zoo::as.yearmon", ..., new_line = FALSE)
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "yearmon"
  )
}

#' @export
#' @method .cstr_construct.yearmon yearmon
.cstr_construct.yearmon.yearmon <- function(x, ...) {
  args <- list(unclass(x))
  code <- .cstr_apply(args, fun = "zoo::yearmon", ..., new_line = FALSE)
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "yearmon"
  )
}

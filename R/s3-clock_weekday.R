#' Constructive options for class 'clock_weekday'
#'
#' These options will be used on objects of class 'clock_weekday'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"weekday"` (default): We build the object using `clock::weekday()` on
#'   day codes, from 1 (Sunday) to 7 (Saturday).
#' * `"next"` : Use the constructor for the next supported class.
#' * `"integer"` : We define as an atomic vector and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_clock_weekday>
#' @export
opts_clock_weekday <- function(constructor = c("weekday", "next", "integer"), ...) {
  .cstr_options("clock_weekday", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct clock_weekday
.cstr_construct.clock_weekday <- function(x, ...) {
  opts <- list(...)$opts$clock_weekday %||% opts_clock_weekday()
  if (is_corrupted_clock_weekday(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.clock_weekday", structure(NA, class = opts$constructor))
}

is_corrupted_clock_weekday <- function(x) {
  !is.integer(x) || !all(as.integer(x) %in% c(1:7, NA))
}

#' @export
#' @method .cstr_construct.clock_weekday weekday
.cstr_construct.clock_weekday.weekday <- function(x, ...) {
  code <- .cstr_apply(list(as.double(x)), "clock::weekday", ...)
  repair_attributes_clock_weekday(x, code, ...)
}

#' @export
#' @method .cstr_construct.clock_weekday integer
.cstr_construct.clock_weekday.integer <- function(x, ...) {
  .cstr_construct.integer(x, ...)
}

repair_attributes_clock_weekday <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("clock_weekday", "vctrs_vctr")
  )
}

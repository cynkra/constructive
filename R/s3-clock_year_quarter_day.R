#' Constructive options for class 'clock_year_quarter_day'
#'
#' These options will be used on objects of class 'clock_year_quarter_day'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"year_quarter_day"` (default): We build the object using `clock::year_quarter_day()`,
#'   providing the components required by the precision, and `start` if it is not the default.
#' * `"next"` : Use the constructor for the next supported class.
#' * `"list"` : We define as a list and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_clock_year_quarter_day>
#' @export
opts_clock_year_quarter_day <- function(constructor = c("year_quarter_day", "next", "list"), ...) {
  .cstr_options("clock_year_quarter_day", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct clock_year_quarter_day
.cstr_construct.clock_year_quarter_day <- function(x, ...) {
  opts <- list(...)$opts$clock_year_quarter_day %||% opts_clock_year_quarter_day()
  if (is_corrupted_clock_year_quarter_day(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.clock_year_quarter_day", structure(NA, class = opts$constructor))
}

is_corrupted_clock_year_quarter_day <- function(x) {
  is_corrupted_clock_calendar(
    x,
    fields = c("year", "quarter", "day", "hour", "minute", "second", "subsecond"),
    start = 1:12
  )
}

#' @export
#' @method .cstr_construct.clock_year_quarter_day year_quarter_day
.cstr_construct.clock_year_quarter_day.year_quarter_day <- function(x, ...) {
  code <- construct_clock_calendar(x, "clock::year_quarter_day", ...)
  repair_attributes_clock_year_quarter_day(x, code, ...)
}

#' @export
#' @method .cstr_construct.clock_year_quarter_day list
.cstr_construct.clock_year_quarter_day.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_clock_year_quarter_day <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("clock_year_quarter_day", "clock_calendar", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
    ignore = c("precision", "start")
  )
}

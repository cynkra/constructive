#' Constructive options for class 'clock_year_month_day'
#'
#' These options will be used on objects of class 'clock_year_month_day'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"year_month_day"` (default): We build the object using `clock::year_month_day()`,
#'   providing the components required by the precision.
#' * `"next"` : Use the constructor for the next supported class.
#' * `"list"` : We define as a list and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_clock_year_month_day>
#' @export
opts_clock_year_month_day <- function(constructor = c("year_month_day", "next", "list"), ...) {
  .cstr_options("clock_year_month_day", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct clock_year_month_day
.cstr_construct.clock_year_month_day <- function(x, ...) {
  opts <- list(...)$opts$clock_year_month_day %||% opts_clock_year_month_day()
  if (is_corrupted_clock_year_month_day(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.clock_year_month_day", structure(NA, class = opts$constructor))
}

is_corrupted_clock_year_month_day <- function(x) {
  is_corrupted_clock_calendar(
    x,
    fields = c("year", "month", "day", "hour", "minute", "second", "subsecond")
  )
}

#' @export
#' @method .cstr_construct.clock_year_month_day year_month_day
.cstr_construct.clock_year_month_day.year_month_day <- function(x, ...) {
  code <- construct_clock_calendar(x, "clock::year_month_day", ...)
  repair_attributes_clock_year_month_day(x, code, ...)
}

#' @export
#' @method .cstr_construct.clock_year_month_day list
.cstr_construct.clock_year_month_day.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_clock_year_month_day <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("clock_year_month_day", "clock_calendar", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
    ignore = "precision"
  )
}

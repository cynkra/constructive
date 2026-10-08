#' Constructive options for class 'clock_naive_time'
#'
#' These options will be used on objects of class 'clock_naive_time'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as_naive_time"` (default): We build the object using `clock::as_naive_time()`
#'   on a `clock::year_month_day()` call of the same precision.
#' * `"next"` : Use the constructor for the next supported class.
#' * `"list"` : We define as a list and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_clock_naive_time>
#' @export
opts_clock_naive_time <- function(constructor = c("as_naive_time", "next", "list"), ...) {
  .cstr_options("clock_naive_time", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct clock_naive_time
.cstr_construct.clock_naive_time <- function(x, ...) {
  opts <- list(...)$opts$clock_naive_time %||% opts_clock_naive_time()
  if (is_corrupted_clock_naive_time(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.clock_naive_time", structure(NA, class = opts$constructor))
}

is_corrupted_clock_naive_time <- function(x) {
  # time points have a precision of at least "day"
  is_corrupted_clock_int64(x, precision = 4:10) || !identical(attr(x, "clock"), 1L)
}

#' @export
#' @method .cstr_construct.clock_naive_time as_naive_time
.cstr_construct.clock_naive_time.as_naive_time <- function(x, ...) {
  code <- construct_clock_time_point(x, "clock::as_naive_time", ...)
  repair_attributes_clock_naive_time(x, code, ...)
}

#' @export
#' @method .cstr_construct.clock_naive_time list
.cstr_construct.clock_naive_time.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_clock_naive_time <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("clock_naive_time", "clock_time_point", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
    ignore = c("precision", "clock")
  )
}

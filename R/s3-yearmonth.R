#' Constructive options for class 'yearmonth'
#'
#' These options will be used on objects of class 'yearmonth'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"yearmonth"` (default): We build the object using `tsibble::yearmonth()`
#'   on a string in the format `"2024 Jan"`. If the object contains `NA`s or
#'   years that can't be parsed from a string, we use a `Date` object as an
#'   input instead.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_yearmonth>
#' @export
opts_yearmonth <- function(constructor = c("yearmonth", "next"), ...) {
  .cstr_options("yearmonth", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct yearmonth
.cstr_construct.yearmonth <- function(x, ...) {
  opts <- list(...)$opts$yearmonth %||% opts_yearmonth()
  if (is_corrupted_yearmonth(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.yearmonth", structure(NA, class = opts$constructor))
}

is_corrupted_yearmonth <- function(x) {
  if (!is.double(x) || any(is.nan(x))) return(TRUE)
  x_dbl <- as.double(x)
  # the underlying data is the number of days since epoch of the first day of the month
  is_first_day <- x_dbl %% 1 == 0 & as.POSIXlt(.Date(x_dbl))$mday == 1
  !isTRUE(all(is.na(x_dbl) | is_first_day))
}

#' @export
#' @method .cstr_construct.yearmonth yearmonth
.cstr_construct.yearmonth.yearmonth <- function(x, ...) {
  dates <- .Date(as.double(x))
  dates_lt <- as.POSIXlt(dates)
  years <- dates_lt$year + 1900
  # tsibble parses strings with {anytime}, which doesn't support years before 1400,
  # and NA strings fail too, in these cases we use a Date input
  if (anyNA(years) || any(years < 1400 | years > 9999)) {
    args <- list(dates)
  } else {
    # month.abb rather than format() to be independent of the locale
    args <- list(paste(years, month.abb[dates_lt$mon + 1]))
  }
  code <- .cstr_apply(args, fun = "tsibble::yearmonth", ..., new_line = FALSE)
  repair_attributes_yearmonth(x, code, ...)
}

repair_attributes_yearmonth <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("yearmonth", "vctrs_vctr"),
    repair_names = TRUE
  )
}

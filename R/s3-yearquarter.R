#' Constructive options for class 'yearquarter'
#'
#' These options will be used on objects of class 'yearquarter'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"yearquarter"` (default): We build the object using `tsibble::yearquarter()`
#'   on a string in the format `"2024 Q1"`. If the object contains `NA`s or
#'   years that can't be parsed from a string, we use a `Date` object as an
#'   input instead.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_yearquarter>
#' @export
opts_yearquarter <- function(constructor = c("yearquarter", "next"), ...) {
  .cstr_options("yearquarter", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct yearquarter
.cstr_construct.yearquarter <- function(x, ...) {
  opts <- list(...)$opts$yearquarter %||% opts_yearquarter()
  if (is_corrupted_yearquarter(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.yearquarter", structure(NA, class = opts$constructor))
}

is_corrupted_yearquarter <- function(x) {
  fiscal_start <- attr(x, "fiscal_start")
  if (!is.numeric(fiscal_start) || length(fiscal_start) != 1 || !fiscal_start %in% 1:12) return(TRUE)
  if (!is.double(x) || any(is.nan(x))) return(TRUE)
  x_dbl <- as.double(x)
  # the underlying data is the number of days since epoch of the first day of
  # the quarter, quarters start on month `fiscal_start`
  dates_lt <- as.POSIXlt(.Date(x_dbl))
  is_first_day <- x_dbl %% 1 == 0 & dates_lt$mday == 1 & (dates_lt$mon + 1 - fiscal_start) %% 3 == 0
  !isTRUE(all(is.na(x_dbl) | is_first_day))
}

#' @export
#' @method .cstr_construct.yearquarter yearquarter
.cstr_construct.yearquarter.yearquarter <- function(x, ...) {
  # tsibble can't parse empty or NA strings, or years that don't have 4 digits,
  # in these cases we use a Date input
  use_date <- !length(x) || anyNA(x)
  if (!use_date) {
    x_chr <- unname(getFromNamespace("format.yearquarter", "tsibble")(x))
    use_date <- !all(grepl("^[0-9]{4} ", x_chr))
  }
  args <- if (use_date) list(.Date(as.double(x))) else list(x_chr)
  fiscal_start <- attr(x, "fiscal_start")
  if (!identical(fiscal_start, 1)) args$fiscal_start <- fiscal_start
  code <- .cstr_apply(args, fun = "tsibble::yearquarter", ..., new_line = FALSE)
  repair_attributes_yearquarter(x, code, ...)
}

repair_attributes_yearquarter <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = "fiscal_start",
    idiomatic_class = c("yearquarter", "vctrs_vctr"),
    repair_names = TRUE
  )
}

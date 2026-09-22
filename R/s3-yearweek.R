#' Constructive options for class 'yearweek'
#'
#' These options will be used on objects of class 'yearweek'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"yearweek"` (default): We build the object using `tsibble::yearweek()`
#'   on a string in the format `"2024 W01"`. If the object contains `NA`s or
#'   years that can't be parsed from a string, we use a `Date` object as an
#'   input instead.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_yearweek>
#' @export
opts_yearweek <- function(constructor = c("yearweek", "next"), ...) {
  .cstr_options("yearweek", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct yearweek
.cstr_construct.yearweek <- function(x, ...) {
  opts <- list(...)$opts$yearweek %||% opts_yearweek()
  if (is_corrupted_yearweek(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.yearweek", structure(NA, class = opts$constructor))
}

is_corrupted_yearweek <- function(x) {
  week_start <- attr(x, "week_start")
  if (!is.numeric(week_start) || length(week_start) != 1 || !week_start %in% 1:7) return(TRUE)
  if (!is.double(x) || any(is.nan(x))) return(TRUE)
  x_dbl <- as.double(x)
  # the underlying data is the number of days since epoch of the first day of
  # the week, `week_start` is 1 for Monday to 7 for Sunday
  is_first_day <- x_dbl %% 1 == 0 & as.POSIXlt(.Date(x_dbl))$wday == week_start %% 7
  !isTRUE(all(is.na(x_dbl) | is_first_day))
}

#' @export
#' @method .cstr_construct.yearweek yearweek
.cstr_construct.yearweek.yearweek <- function(x, ...) {
  # tsibble can't parse NA strings, or years that don't have 4 digits, and
  # can't format empty objects, in these cases we use a Date input
  use_date <- !length(x) || anyNA(x)
  if (!use_date) {
    x_chr <- unname(getFromNamespace("format.yearweek", "tsibble")(x))
    use_date <- !all(grepl("^[0-9]{4} ", x_chr))
  }
  args <- if (use_date) list(.Date(as.double(x))) else list(x_chr)
  week_start <- attr(x, "week_start")
  if (!identical(week_start, 1)) args$week_start <- week_start
  code <- .cstr_apply(args, fun = "tsibble::yearweek", ..., new_line = FALSE)
  repair_attributes_yearweek(x, code, ...)
}

repair_attributes_yearweek <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = "week_start",
    idiomatic_class = c("yearweek", "vctrs_vctr"),
    repair_names = TRUE
  )
}

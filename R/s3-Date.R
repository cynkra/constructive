constructors$Date <- new.env()

#' Constructive options class 'Date'
#'
#' These options will be used on objects of class 'date'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as.Date"` (default): We wrap a character vector with `as.Date()`, if the date
#'   is infinite it cannot be converted to character and we wrap a numeric vector and
#'   provide an `origin` argument.
#' * `"as_date"` : Similar as above but using `lubridate::as_date()`, the only difference is
#'   that we never need to supply `origin`.
#' * `"date"` : Similar as above but using `lubridate::date()`, it doesn't support
#'   infinite dates so we fall back on `lubridate::as_date()` when we encounter them.
#' * `"new_date"` : We wrap a numeric vector with `vctrs::new_date()`
#' * `"as.Date.numeric"` : We wrap a numeric vector with `as.Date()` and use the
#'   provided `origin`
#' * `"as_date.numeric"` : Same as above but using `lubridate::as_date()` and use the
#'   provided `origin`
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"atomic"` : We define as an atomic vector and repair attributes
#'
#' If the data is not appropriate for a constructor we fall back to another one
#' appropriately.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#' @param origin Origin to be used, ignored when irrelevant.
#'
#' @return An object of class <constructive_options/constructive_options_Date>
#' @export
opts_Date <- function(constructor = c("as.Date", "as_date", "date", "new_date", "as.Date.numeric", "as_date.numeric", "next", "atomic"), ..., origin = "1970-01-01") {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "Date"),
    check_dots_empty()
  )
  .cstr_options("Date", constructor = constructor, origin = origin)
}

#' @export
.cstr_construct.Date <- function(x, opts, ...) {
  opts_local <- opts$Date %||% opts_Date()
  if (is_corrupted_Date(x) || opts_local$constructor == "next") return(NextMethod())
  constructor <- constructors$Date[[opts_local$constructor]]
  constructor(x, opts = opts, ..., origin = opts_local$origin)
}

is_corrupted_Date <- function(x) {
  typeof(x) != "double"
}

constructors$Date$as.Date <- function(x, ..., origin = "1970-01-01") {
  compatible_with_char <-
    all(rlang::is_integerish(x) & (is.finite(x) | (is.na(x) & !is.nan(x))))
  if (!compatible_with_char || all(is.na(x))) {
    return(constructors$Date$as.Date.numeric(x, ..., origin = origin))
  }
  code <- .cstr_apply(list(format(x)),  "as.Date", ..., new_line = FALSE)
  repair_attributes_Date(x, code, ...)
}

constructors$Date$date <- function(x, ..., origin) {
  compatible_with_char <-
    all(rlang::is_integerish(x) & (is.finite(x) | (is.na(x) & !is.nan(x))))
  if (!compatible_with_char || all(is.na(x))) {
    return(constructors$Date$as_date.numeric(x, ..., origin = origin))
  }
  code <- .cstr_apply(list(format(x)),  "lubridate::date", ..., new_line = FALSE)
  repair_attributes_Date(x, code, ...)
}

constructors$Date$as_date <- function(x, ..., origin) {
  compatible_with_char <-
    all(rlang::is_integerish(x) & (is.finite(x) | (is.na(x) & !is.nan(x))))
  if (!compatible_with_char || all(is.na(x))) {
    return(constructors$Date$as_date.numeric(x, ..., origin = origin))
  }
  code <- .cstr_apply(list(format(x)),  "lubridate::as_date", ..., new_line = FALSE)
  repair_attributes_Date(x, code, ...)
}

constructors$Date$as.Date.numeric <- function(x, ..., origin) {
  x_dbl <- unclass(x)
  if (!any(is.finite(x))) {
    # as.Date will deal with logical NA so we make it more compact
    if (all(is.na(x) & !is.nan(x))) x_dbl <- as.logical(x)
    code <- .cstr_apply(list(x_dbl), "as.Date", ..., new_line = FALSE)
  } else {
    if (origin != "1970-01-01") x_dbl <- x_dbl - as.numeric(as.Date(origin))
    code <- .cstr_apply(list(x_dbl, origin = origin), "as.Date", ..., new_line = FALSE)
  }
  repair_attributes_Date(x, code, ...)
}

constructors$Date$as_date.numeric <- function(x, ..., origin) {
  x_dbl <- unclass(x)
  if (!any(is.finite(x))) {
    # as_date will deal with logical NA so we make it more compact
    if (all(is.na(x) & !is.nan(x))) x_dbl <- as.logical(x)
    code <- .cstr_apply(list(x_dbl), "lubridate::as_date", ..., new_line = FALSE)
  } else {
    if (origin != "1970-01-01") {
      x_dbl <- x_dbl - as.numeric(as.Date(origin))
      code <- .cstr_apply(list(x_dbl, origin = origin), "lubridate::as_date", ..., new_line = FALSE)
    } else {
      code <- .cstr_apply(list(x_dbl), "lubridate::as_date", ..., new_line = FALSE)
    }
  }
  repair_attributes_Date(x, code, ...)
}

constructors$Date$new_date <- function(x, ..., origin) {
  code <- .cstr_apply(list(unclass(x)), "vctrs::new_date", ..., new_line = FALSE)
  repair_attributes_Date(x, code, ...)
}

constructors$Date$atomic <- function(x, ..., origin) {
  .cstr_construct.atomic(x, ...)
}

repair_attributes_Date <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "Date"
  )
}

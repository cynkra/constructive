constructors$Date <- new.env()

#' Constructive options class 'Date'
#'
#' These options will be used on objects of class 'date'.
#'
#' Depending on `constructor`, we construct the environment as follows:
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
#' @param constructor String. Name of the function used to construct the environment.
#' @inheritParams opts_atomic
#' @param origin Origin to be used, ignored when irrelevant.
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_Date <- function(constructor = c("as.Date", "as_date", "date", "new_date", "as.Date.numeric", "as_date.numeric", "next", "atomic"), ..., origin = "1970-01-01") {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "Date"),
    ellipsis::check_dots_empty()
  )
  .cstr_options("Date", constructor = constructor, origin = origin)
}

#' @export
.cstr_construct.Date <- function(x, ...) {
  opts <- .cstr_fetch_opts("Date", ...)
  if (is_corrupted_Date(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$Date[[opts$constructor]]
  constructor(x, ..., origin = opts$origin)
}

is_corrupted_Date <- function(x) {
  typeof(x) != "double"
}

constructors$Date$as.Date <- function(x, ..., origin = "1970-01-01") {
  if (any(is.infinite(x)) && any(is.finite(x))) {
    x_dbl <- unclass(x)
    if (origin != "1970-01-01") x_dbl <- x_dbl - as.numeric(as.Date(origin))
    code <- .cstr_apply(list(x_dbl, origin = origin), "as.Date", ..., new_line = FALSE)
  } else {
    code <- .cstr_apply(list(format(x)),  "as.Date", ..., new_line = FALSE)
  }
  repair_attributes_Date(x, code, ...)
}

constructors$Date$date <- function(x, ..., origin) {
  if (any(is.infinite(x)) && any(is.finite(x))) {
    return(constructors$Date$as_date(x, ..., origin = origin))
  } else {
    code <- .cstr_apply(list(format(x)),  "lubridate::date", ..., new_line = FALSE)
  }
  repair_attributes_Date(x, code, ...)
}

constructors$Date$as_date <- function(x, ..., origin) {
  if (any(is.infinite(x)) && any(is.finite(x))) {
    x_dbl <- unclass(x)
    if (origin == "1970-01-01") {
      code <- .cstr_apply(list(x_dbl), "lubridate::as_date", ..., new_line = FALSE)
    } else {
      x_dbl <- x_dbl - as.numeric(as.Date(origin))
      code <- .cstr_apply(list(x_dbl, origin = origin), "lubridate::as_date", ..., new_line = FALSE)
    }
  } else {
    code <- .cstr_apply(list(format(x)),  "lubridate::as_date", ..., new_line = FALSE)
  }
  repair_attributes_Date(x, code, ...)
}

constructors$Date$as.Date.numeric <- function(x, ..., origin) {
  x_dbl <- unclass(x)
  if (origin != "1970-01-01") x_dbl <- x_dbl - as.numeric(as.Date(origin))
  code <- .cstr_apply(list(x_dbl, origin = origin), "as.Date", ..., new_line = FALSE)
  repair_attributes_Date(x, code, ...)
}

constructors$Date$as_date.numeric <- function(x, ..., origin) {
  x_dbl <- unclass(x)
  if (origin != "1970-01-01") {
    x_dbl <- x_dbl - as.numeric(as.Date(origin))
  code <- .cstr_apply(list(x_dbl, origin = origin), "lubridate::as_date", ..., new_line = FALSE)
  } else {
    code <- .cstr_apply(list(x_dbl), "lubridate::as_date", ..., new_line = FALSE)
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

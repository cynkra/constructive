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
#'
#' @param constructor String. Name of the function used to construct the environment.
#' @param origin Origin to be used, ignored when irrelevant.
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_Date <- function(constructor = c("as.Date", "as_date", "date", "new_date", "as.Date.numeric", "as_date.numeric"), origin = "1970-01-01") {
  constructor <- rlang::arg_match(constructor)
  structure(
    class = c("constructive_options", "constructive_options_Date"),
    list(constructor = constructor, origin = origin)
  )
}

#' @export
construct_idiomatic.Date <- function(x, ...) {
  opts <- fetch_opts("Date", ...)
  origin <- opts$origin
  constructor <- opts$constructor

  if (constructor == "as.Date") {
    if (any(is.infinite(x)) && any(is.finite(x))) {
      x_dbl <- unclass(x)
      if (origin != "1970-01-01") x_dbl <- x_dbl - as.numeric(as.Date(origin))
      code <- construct_apply(list(x_dbl, origin = origin), constructor, ..., new_line = FALSE)
    } else {
      code <- construct_apply(list(format(x)),  "as.Date", ..., new_line = FALSE)
    }
    return(code)
  }

  if (constructor %in% c("as_date", "date")) {
    constructor <- paste0("lubridate::", constructor)
    if (any(is.infinite(x)) && any(is.finite(x))) {
      x_dbl <- unclass(x)
      if (origin == "1970-01-01") {
        code <- construct_apply(list(x_dbl), "lubridate::as_date", ..., new_line = FALSE)
      } else {
        x_dbl <- x_dbl - as.numeric(as.Date(origin))
        code <- construct_apply(list(x_dbl, origin = origin), "lubridate::as_date", ..., new_line = FALSE)
      }
    } else {
      code <- construct_apply(list(format(x)),  constructor, ..., new_line = FALSE)
    }
    return(code)
  }

  if (constructor == "as.Date.numeric") {
    x_dbl <- unclass(x)
    if (origin != "1970-01-01") x_dbl <- x_dbl - as.numeric(as.Date(origin))
    code <- construct_apply(list(x_dbl, origin = origin), "as.Date", ..., new_line = FALSE)
    return(code)
  }

  if (constructor == "as_date.numeric") {
    x_dbl <- unclass(x)
    if (origin != "1970-01-01") x_dbl <- x_dbl - as.numeric(as.Date(origin))
    code <- construct_apply(list(x_dbl), "lubridate::as_date", ..., new_line = FALSE)
    return(code)
  }

  if (constructor == "new_date") {
    code <- construct_apply(list(unclass(x)), "vctrs::new_date", ..., new_line = FALSE)
    return(code)
  }
}

#' @export
repair_attributes.Date <- function(x, code, ..., pipe = "base") {
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    idiomatic_class = "Date"
  )
}

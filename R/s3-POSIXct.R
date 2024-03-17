constructors$POSIXct <- new.env()

#' Constructive options for class 'POSIXct'
#'
#' These options will be used on objects of class 'POSIXct'.
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"as.POSIXct"` (default): Build the object using a `as.POSIXct()` call on a
#' character vector.
#' * `".POSIXct"` : Build the object using a `.POSIXct()` call on a numeric vector.
#' * `"as_datetime"` : Build the object using a `lubridate::as_datetime()` call on
#' a character vector.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"atomic"` : We define as an atomic vector and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @param origin Origin to be used, ignored when irrelevant.
#'
#' @return An object of class <constructive_options/constructive_options_factor>
#' @export
opts_POSIXct <- function(constructor = c("as.POSIXct", ".POSIXct", "as_datetime", "as.POSIXct.numeric", "as_datetime.numeric", "next", "atomic"), ..., origin = "1970-01-01") {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "POSIXct"),
    check_dots_empty()
  )
  .cstr_options("POSIXct", constructor = constructor, origin = origin)
}

#' @export
.cstr_construct.POSIXct <- function(x, ...) {
  opts <- .cstr_fetch_opts("POSIXct", ...)
  if (is_corrupted_POSIXct(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$POSIXct[[opts$constructor]]
  constructor(x, ..., origin = opts$origin)
}

is_corrupted_POSIXct <- function(x) {
  # TODO
  FALSE
}

constructors$POSIXct$.POSIXct <- function(x, ..., origin) {
  args <- list(as.numeric(x), tz = attr(x, "tzone"))
  code <- .cstr_apply(args, ".POSIXct", new_line = TRUE, ...)
  repair_attributes_POSIXct(x, code, ...)
}

constructors$POSIXct$as.POSIXct.numeric <- function(x, ..., origin) {
  args <- list(as.numeric(x) - as.numeric(as.POSIXct(origin, "GMT")), tz = attr(x, "tzone"), origin = origin)
  code <- .cstr_apply(args, "as.POSIXct", new_line = TRUE, ...)
  repair_attributes_POSIXct(x, code, ...)
}

constructors$POSIXct$as_datetime.numeric <- function(x, ..., origin) {
  tzone <- attr(x, "tzone")
  args <- list(as.numeric(x))
  if (is.null(tzone) || tzone != "UTC") args <- c(args, list(tz = tzone))
  origin_dbl <- as.numeric(as.POSIXct(origin, "UTC"))
  if (origin_dbl != 0) {
    args[[1]] <- args[[1]] - origin_dbl
    args <- c(args, list(origin = origin))
  }
  code <- .cstr_apply(args, "lubridate::as_datetime", new_line = TRUE, ...)
  repair_attributes_POSIXct(x, code, ...)
}

constructors$POSIXct$as_datetime <- function(x, ..., origin) {
  tzone <- attr(x, "tzone")
  x_chr <- format(x)
  split_s <- as.numeric(x) %% 1
  dec_lgl <- split_s != 0 & !is.na(x)
  x_chr[dec_lgl] <- paste0(x_chr[dec_lgl], sub("^0", "", format(split_s[dec_lgl], digits = 5)))
  args <- list(x_chr)
  if (is.null(tzone) || tzone != "UTC") args <- c(args, list(tz = tzone))
  code <- .cstr_apply(args, "lubridate::as_datetime", new_line = TRUE, ...)
  repair_attributes_POSIXct(x, code, ...)
}

constructors$POSIXct$as_datetime <- function(x, ..., origin) {
  tzone <- attr(x, "tzone")
  x_chr <- format(x)
  split_s <- as.numeric(x) %% 1
  dec_lgl <- split_s != 0 & !is.na(x)
  x_chr[dec_lgl] <- paste0(x_chr[dec_lgl], sub("^0", "", format(split_s[dec_lgl], digits = 5)))
  args <- list(x_chr)
  if (is.null(tzone) || tzone != "UTC") args <- c(args, list(tz = tzone))
  code <- .cstr_apply(args, "lubridate::as_datetime", new_line = TRUE, ...)
  repair_attributes_POSIXct(x, code, ...)
}

#' @export
constructors$POSIXct$as.POSIXct <- function(x, ..., origin) {
  tzone <- attr(x, "tzone")

  x_chr <- format(x)
  split_s <- as.numeric(x) %% 1
  dec_lgl <- split_s != 0 & !is.na(x)
  x_chr[dec_lgl] <- paste0(x_chr[dec_lgl], sub("^0", "", format(split_s[dec_lgl], digits = 5)))
  args <- list(x_chr)
  if (!is.null(tzone) && tzone != "") {
    args <- c(args, list(tz = tzone))
  }
  code <- .cstr_apply(args, "as.POSIXct", new_line = TRUE, ...)
  repair_attributes_POSIXct(x, code, ...)
}

#' @export
constructors$POSIXct$atomic <- function(x, ..., origin) {
  .cstr_construct.default(x, ...)
}

repair_attributes_POSIXct <- function(x, code, ..., pipe = NULL) {
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    idiomatic_class = c("POSIXct", "POSIXt"),
    ignore = "tzone",
    remove = if (is.null(attr(x, "tzone"))) "tzone" else NULL
  )
}

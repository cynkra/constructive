#' Constructive options for class 'POSIXct'
#'
#' These options will be used on objects of class 'POSIXct'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as.POSIXct"` (default): Build the object using a `as.POSIXct()` call on a
#' character vector.
#' * `".POSIXct"` : Build the object using a `.POSIXct()` call on a numeric vector.
#' * `"as_datetime"` : Build the object using a `lubridate::as_datetime()` call on
#' a character vector.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"atomic"` : We define as an atomic vector and repair attributes.
#'
#' If the data is not appropriate for a constructor we fall back to another one
#' appropriately. In particular corrupted POSIXct objects such as those defined
#' on top of integers (or worse) are all constructed with the `".POSIXct"` constructor.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @param origin Origin to be used, ignored when irrelevant.
#'
#' @return An object of class <constructive_options/constructive_options_POSIXct>
#' @export
opts_POSIXct <- function(constructor = c("as.POSIXct", ".POSIXct", "as_datetime", "as.POSIXct.numeric", "as_datetime.numeric", "next", "atomic"), ..., origin = "1970-01-01") {
  .cstr_options("POSIXct", constructor = constructor[[1]], ..., origin = origin)
}

#' @export
#' @method .cstr_construct POSIXct
.cstr_construct.POSIXct <- function(x, ...) {
  opts <- list(...)$opts$POSIXct %||% opts_POSIXct()
  if (opts$constructor == "next") return(NextMethod())
  if (is_corrupted_POSIXct(x)) {
    # .POSIXct just applies attributes
    .cstr_construct.POSIXct..POSIXct(x, ...)
  } else {
    UseMethod(".cstr_construct.POSIXct", structure(NA, class = opts$constructor))
  }
}

is_corrupted_POSIXct <- function(x) {
  !is.double(x)
}

#' @export
#' @method .cstr_construct.POSIXct .POSIXct
.cstr_construct.POSIXct..POSIXct <- function(x, ...) {
  x_bare <- x
  attributes(x_bare) <- NULL
  args <- list(x_bare)
  args$tz <- attr(x, "tzone")
  code <- .cstr_apply(args, ".POSIXct", new_line = TRUE, ...)
  repair_attributes_POSIXct(x, code, ..., remove_null_tz = FALSE)
}

#' @export
#' @method .cstr_construct.POSIXct as.POSIXct.numeric
.cstr_construct.POSIXct.as.POSIXct.numeric <- function(x, ...) {
  opts <- list(...)$opts$POSIXct %||% opts_POSIXct()
  args <- list(
    as.numeric(x) - as.numeric(as.POSIXct(opts$origin, "GMT")),
    tz = attr(x, "tzone"),
    origin = opts$origin
  )
  code <- .cstr_apply(args, "as.POSIXct", new_line = TRUE, ...)
  repair_attributes_POSIXct(x, code, ...)
}

#' @export
#' @method .cstr_construct.POSIXct as_datetime.numeric
.cstr_construct.POSIXct.as_datetime.numeric <- function(x, ...) {
  opts <- list(...)$opts$POSIXct %||% opts_POSIXct()
  tzone <- attr(x, "tzone")
  args <- list(as.numeric(x))
  if (is.null(tzone) || tzone != "UTC") args <- c(args, list(tz = tzone))
  origin_dbl <- as.numeric(as.POSIXct(opts$origin, "UTC"))
  if (origin_dbl != 0) {
    args[[1]] <- args[[1]] - origin_dbl
    args <- c(args, list(origin = opts$origin))
  }
  code <- .cstr_apply(args, "lubridate::as_datetime", new_line = TRUE, ...)
  repair_attributes_POSIXct(x, code, ...)
}

#' @export
#' @method .cstr_construct.POSIXct as_datetime
.cstr_construct.POSIXct.as_datetime <- function(x, ...) {
  tzone <- attr(x, "tzone")
  x_chr <- format(x)
  split_s <- as.numeric(x) %% 1
  dec_lgl <- split_s != 0 & !is.na(x)
  dec_seconds <- vapply(strip(x[dec_lgl]), .cstr_construct, character(1))
  dec_seconds <- sub("^.*(\\..*)", "\\1", dec_seconds)
  x_chr[dec_lgl] <- paste0(x_chr[dec_lgl], dec_seconds)
  args <- list(x_chr)
  if (is.null(tzone) || tzone != "UTC") args <- c(args, list(tz = tzone))
  code <- .cstr_apply(args, "lubridate::as_datetime", new_line = TRUE, ...)
  repair_attributes_POSIXct(x, code, ...)
}

#' @export
#' @method .cstr_construct.POSIXct as_datetime
.cstr_construct.POSIXct.as_datetime <- function(x, ...) {
  opts <- list(...)$opts$POSIXct %||% opts_POSIXct()
  tzone <- attr(x, "tzone")
  x_chr <- format(x)
  split_s <- as.numeric(x) %% 1
  dec_lgl <- split_s != 0 & !is.na(x)
  dec_seconds <- vapply(strip(x[dec_lgl]), .cstr_construct, character(1))
  dec_seconds <- sub("^.*(\\..*)", "\\1", dec_seconds)
  x_chr[dec_lgl] <- paste0(x_chr[dec_lgl], dec_seconds)
  args <- list(x_chr)
  if (is.null(tzone) || tzone != "UTC") args <- c(args, list(tz = tzone))
  code <- .cstr_apply(args, "lubridate::as_datetime", new_line = TRUE, ...)
  repair_attributes_POSIXct(x, code, ...)
}

#' @export
#' @method .cstr_construct.POSIXct as.POSIXct
.cstr_construct.POSIXct.as.POSIXct <- function(x, ...) {
  tzone <- attr(x, "tzone")

  x_chr <- format(x)
  split_s <- as.numeric(x) %% 1
  dec_lgl <- split_s != 0 & !is.na(x)
  dec_seconds <- vapply(strip(x[dec_lgl]), .cstr_construct, character(1))
  dec_seconds <- sub("^.*(\\..*)", "\\1", dec_seconds)
  x_chr[dec_lgl] <- paste0(x_chr[dec_lgl], dec_seconds)
  args <- list(x_chr)
  if (!is.null(tzone) && tzone != "") {
    args <- c(args, list(tz = tzone))
  }
  code <- .cstr_apply(args, "as.POSIXct", new_line = TRUE, ...)
  repair_attributes_POSIXct(x, code, ...)
}

#' @export
#' @method .cstr_construct.POSIXct atomic
.cstr_construct.POSIXct.atomic <- function(x, ...) {
  .cstr_construct.default(x, ...)
}

repair_attributes_POSIXct <- function(x, code, ..., pipe = NULL, remove_null_tz = TRUE) {
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    idiomatic_class = c("POSIXct", "POSIXt"),
    ignore = "tzone",
    remove = if (remove_null_tz && is.null(attr(x, "tzone"))) "tzone"
  )
}

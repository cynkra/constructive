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
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @param origin Origin to be used, ignored when irrelevant.
#'
#' @return An object of class <constructive_options/constructive_options_factor>
#' @export
opts_POSIXct <- function(constructor = c("as.POSIXct", ".POSIXct", "as_datetime", "as.POSIXct.numeric", "as_datetime.numeric"), ..., origin = "1970-01-01") {
  combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  structure(
    class = c("constructive_options", "constructive_options_POSIXct"),
    list(constructor = constructor, origin = origin)
  )
}

#' @export
construct_idiomatic.POSIXct <- function(x, ...) {
  opts <- fetch_opts("POSIXct", ...)
  constructor <- opts$constructor
  origin <- opts$origin
  tzone <- attr(x, "tzone")

  if (constructor == ".POSIXct") {
    args <- list(as.numeric(x), tz = tzone)
    code <- construct_apply(args, ".POSIXct", new_line = TRUE, ...)
    return(code)
  }

  if (constructor == "as.POSIXct.numeric") {
    args <- list(as.numeric(x) - as.numeric(as.POSIXct(origin, "GMT")), tz = tzone, origin = origin)
    code <- construct_apply(args, "as.POSIXct", new_line = TRUE, ...)
    return(code)
  }

  if (constructor == "as_datetime.numeric") {
    args <- list(as.numeric(x))
    if (is.null(tzone) || tzone != "UTC") args <- c(args, list(tz = tzone))
    origin_dbl <- as.numeric(as.POSIXct(origin, "UTC"))
    if (origin_dbl != 0) {
      args[[1]] <- args[[1]] - origin_dbl
      args <- c(args, list(origin = origin))
    }
    code <- construct_apply(args, "lubridate::as_datetime", new_line = TRUE, ...)
    return(code)
  }

  x_chr <- format(x)
  split_s <- as.numeric(x) %% 1
  dec_lgl <- split_s != 0 & !is.na(x)
  x_chr[dec_lgl] <- paste0(x_chr[dec_lgl], sub("^0", "", format(split_s[dec_lgl], digits = 5)))
  args <- list(x_chr)

  if (constructor == "as_datetime") {
    if (is.null(tzone) || tzone != "UTC") args <- c(args, list(tz = tzone))
    code <- construct_apply(args, "lubridate::as_datetime", new_line = TRUE, ...)
    return(code)
  }

  # as.POSIXct
  if (!is.null(tzone) && tzone != "") {
    args <- c(args, list(tz = tzone))
  }
  construct_apply(args, "as.POSIXct", new_line = TRUE, ...)
}

#' @export
repair_attributes.POSIXct <- function(x, code, ..., pipe ="base") {
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    idiomatic_class = c("POSIXct", "POSIXt"),
    ignore = "tzone",
    remove = if (is.null(attr(x, "tzone"))) "tzone" else NULL
  )
}

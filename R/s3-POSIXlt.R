#' Constructive options for class 'POSIXlt'
#'
#' These options will be used on objects of class 'POSIXlt'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as.POSIXlt"` (default): Build the object using a `as.POSIXlt()` call on a
#' character vector.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"list"` : We define as a list and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_POSIXlt>
#' @export
opts_POSIXlt <- function(constructor = c("as.POSIXlt", "next", "list"), ...) {
  .cstr_options("POSIXlt", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct POSIXlt
.cstr_construct.POSIXlt <- function(x, ...) {
  opts <- list(...)$opts$POSIXlt %||% opts_POSIXlt()
  if (is_corrupted_POSIXlt(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.POSIXlt", structure(NA, class = opts$constructor))
}

is_corrupted_POSIXlt <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.POSIXlt as.POSIXlt
.cstr_construct.POSIXlt.as.POSIXlt <- function(x, ...) {
  if (with_versions(R < "4.3.0")) {
    gmtoff <- .subset2(x, "gmtoff")
    from_posixct <- !is.null(gmtoff) && !all(is.na(gmtoff))
    if (from_posixct) {
      code_posixct <- .cstr_construct(as.POSIXct(x), ...)
      code <- .cstr_wrap(code_posixct, "as.POSIXlt", new_line = FALSE)
      return(repair_attributes_POSIXlt(x, code, ...))
    }
  }
  tzone <- attr(x, "tzone")
  x_chr <- format(x)
  split_s <- as.numeric(x) %% 1
  dec_lgl <- split_s != 0 & !is.na(x)
  x_chr[dec_lgl] <- paste0(x_chr[dec_lgl], sub("^0", "", format(split_s[dec_lgl], digits = 5)))
  args <- list(x_chr)
  if (!is.null(tzone) && length(tzone) == 1) {
    args <- c(args, list(tz = tzone))
  }
  code <- .cstr_apply(args, "as.POSIXlt", ..., new_line = TRUE)
  repair_attributes_POSIXlt(x, code, ...)
}

#' @export
#' @method .cstr_construct.POSIXlt list
.cstr_construct.POSIXlt.list <- function(x, ...) {
  code <- .cstr_construct.list(x, ...)
  repair_attributes_POSIXlt(x, code, ...)
}

repair_attributes_POSIXlt <- function(x, code, ..., pipe = NULL) {
  if (with_versions(R >= "4.3.0")) {
    ignore <- c("tzone", "balanced")
  } else {
    ignore <- c("tzone")
  }

  code <- .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    idiomatic_class = c("POSIXlt", "POSIXt"),
    ignore =  ignore
  )
  code
}

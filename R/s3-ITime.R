#' Constructive options for class 'ITime'
#'
#' These options will be used on objects of class 'ITime'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as.ITime"` (default): We build the object using `data.table::as.ITime()`
#'   on a character vector in the format `"HH:MM:SS"`. Negative times and times
#'   of 24 hours or more cannot be built this way so we fall back to the
#'   `"integer"` constructor.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"integer"` : We define as an integer vector and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_ITime>
#' @export
opts_ITime <- function(constructor = c("as.ITime", "next", "integer"), ...) {
  .cstr_options("ITime", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ITime
.cstr_construct.ITime <- function(x, ...) {
  opts <- list(...)$opts$ITime %||% opts_ITime()
  if (is_corrupted_ITime(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ITime", structure(NA, class = opts$constructor))
}

is_corrupted_ITime <- function(x) {
  typeof(x) != "integer"
}

#' @export
#' @method .cstr_construct.ITime as.ITime
.cstr_construct.ITime.as.ITime <- function(x, ...) {
  secs <- as.integer(x)
  if (!all(is.na(secs) | (secs >= 0L & secs < 86400L))) {
    return(.cstr_construct.ITime.integer(x, ...))
  }
  x_chr <- sprintf("%02d:%02d:%02d", secs %/% 3600L, secs %/% 60L %% 60L, secs %% 60L)
  x_chr[is.na(secs)] <- NA
  code <- .cstr_apply(list(x_chr), "data.table::as.ITime", ..., new_line = FALSE)
  repair_attributes_ITime(x, code, ...)
}

#' @export
#' @method .cstr_construct.ITime integer
.cstr_construct.ITime.integer <- function(x, ...) {
  .cstr_construct.integer(x, ...)
}

repair_attributes_ITime <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "ITime",
    repair_names = TRUE
  )
}

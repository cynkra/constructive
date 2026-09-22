#' Constructive options for class 'Duration'
#'
#' These options will be used on objects of class 'Duration'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"dseconds"` (default): We build the object using `lubridate::dseconds()`
#'   on a number of seconds.
#' * `"duration"` : We build the object using `lubridate::duration()`
#'   on a number of seconds.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_Duration>
#' @export
opts_Duration <- function(constructor = c("dseconds", "duration", "next"), ...) {
  .cstr_options("Duration", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct Duration
.cstr_construct.Duration <- function(x, ...) {
  opts <- list(...)$opts$Duration %||% opts_Duration()
  if (is_corrupted_Duration(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.Duration", structure(NA, class = opts$constructor))
}

is_corrupted_Duration <- function(x) {
  !isS4(x) || typeof(x) != "double"
}

#' @export
#' @method .cstr_construct.Duration dseconds
.cstr_construct.Duration.dseconds <- function(x, ...) {
  code <- .cstr_apply(list(x@.Data), "lubridate::dseconds", ..., new_line = FALSE)
  repair_attributes_Duration(x, code, ...)
}

#' @export
#' @method .cstr_construct.Duration duration
.cstr_construct.Duration.duration <- function(x, ...) {
  code <- .cstr_apply(list(x@.Data), "lubridate::duration", ..., new_line = FALSE)
  repair_attributes_Duration(x, code, ...)
}

repair_attributes_Duration <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = names(methods::getSlots(class(x))),
    idiomatic_class = class(x),
    flag_s4 = FALSE
  )
}

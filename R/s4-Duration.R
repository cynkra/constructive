#' Constructive options for class 'Duration'
#'
#' These options will be used on objects of class 'Duration'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"default"` (default): We build the object using the largest of
#'   `lubridate::dweeks()`, `lubridate::ddays()`, `lubridate::dhours()`,
#'   `lubridate::dminutes()` and `lubridate::dseconds()` for which all the
#'   elements are whole numbers and the object is reproduced exactly, so an
#'   hour is built with `lubridate::dhours(1)` rather than
#'   `lubridate::dseconds(3600)`. `NA` elements don't constrain the choice, and
#'   we use `lubridate::dseconds()` if all elements are `NA` or zero, or if the
#'   object is empty. We never use `lubridate::dyears()` and
#'   `lubridate::dmonths()` because they are approximations (365.25 and 30.4375
#'   days), so using them on arbitrary durations would be surprising.
#' * `"dseconds"` : We build the object using `lubridate::dseconds()`
#'   on a number of seconds.
#' * `"duration"` : We build the object using `lubridate::duration()`
#'   on a number of seconds.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_Duration>
#' @export
opts_Duration <- function(constructor = c("default", "dseconds", "duration", "next"), ...) {
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
#' @method .cstr_construct.Duration default
.cstr_construct.Duration.default <- function(x, ...) {
  data <- x@.Data
  # `lubridate::dyears()` and `lubridate::dmonths()` are left out on purpose,
  # they are approximations (365.25 and 30.4375 days) so using them on
  # arbitrary durations would be surprising
  units <- c(dweeks = 604800, ddays = 86400, dhours = 3600, dminutes = 60)
  # a larger unit would be arbitrary if we have nothing but zeroes and `NA`s
  if (any(data != 0, na.rm = TRUE)) {
    for (nm in names(units)) {
      unit <- units[[nm]]
      values <- data / unit
      whole <- all(values %% 1 == 0 | is.na(data))
      # non finite values are only whole numbers of seconds, and we make sure
      # the unit reproduces the object exactly before using it
      if (!isTRUE(whole) || !identical(values * unit, data)) next
      code <- .cstr_apply(list(values), paste0("lubridate::", nm), ..., new_line = FALSE)
      return(repair_attributes_Duration(x, code, ...))
    }
  }
  .cstr_construct.Duration.dseconds(x, ...)
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

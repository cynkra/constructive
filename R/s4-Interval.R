#' Constructive options for class 'Interval'
#'
#' These options will be used on objects of class 'Interval'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"interval"` (default): We build the object using `lubridate::interval()`
#'   on "POSIXct" `start` and `end` vectors. If the end dates can't be computed
#'   in a way that reproduces the object exactly (floating point issues), or if
#'   the time zone of the start dates differs from the `tzone` slot, we fall
#'   back to the next constructor.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_Interval>
#' @export
opts_Interval <- function(constructor = c("interval", "next"), ...) {
  .cstr_options("Interval", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct Interval
.cstr_construct.Interval <- function(x, ...) {
  opts <- list(...)$opts$Interval %||% opts_Interval()
  if (is_corrupted_Interval(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.Interval", structure(NA, class = opts$constructor))
}

is_corrupted_Interval <- function(x) {
  if (!isS4(x) || typeof(x) != "double") return(TRUE)
  start <- x@start
  tzone <- x@tzone
  if (
    !identical(oldClass(start), c("POSIXct", "POSIXt")) ||
    typeof(start) != "double" ||
    length(start) != length(x@.Data) ||
    !is.character(tzone) ||
    length(tzone) != 1 ||
    # lubridate::interval() drops the "tzone" attribute of `start` when `tzone = ""`
    !identical(attr(start, "tzone") %||% "", tzone)
  ) {
    return(TRUE)
  }
  # lubridate::interval() computes the span as `end - start`, check that we
  # can build `end` so this gives back the span exactly
  end <- start + x@.Data
  !identical(as.numeric(end) - as.numeric(start), x@.Data)
}

#' @export
#' @method .cstr_construct.Interval interval
.cstr_construct.Interval.interval <- function(x, ...) {
  if (length(x@.Data)) {
    start <- x@start
    attr(start, "tzone") <- x@tzone
    end <- start + x@.Data
    attr(end, "tzone") <- x@tzone
    args <- list(start, end)
  } else if (x@tzone == "UTC") {
    args <- list()
  } else {
    args <- list(tzone = x@tzone)
  }
  code <- .cstr_apply(args, "lubridate::interval", ...)
  repair_attributes_Interval(x, code, ...)
}

repair_attributes_Interval <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = names(methods::getSlots(class(x))),
    idiomatic_class = class(x),
    flag_s4 = FALSE
  )
}

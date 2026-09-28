#' Constructive options for class 'clock_duration'
#'
#' These options will be used on objects of class 'clock_duration'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"duration"` (default): We build the object using the `clock::duration_*()`
#'   helper matching the precision, e.g. `clock::duration_days()`. Durations
#'   that don't fit in an integer are constructed with the `"list"` constructor.
#' * `"next"` : Use the constructor for the next supported class.
#' * `"list"` : We define as a list and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_clock_duration>
#' @export
opts_clock_duration <- function(constructor = c("duration", "next", "list"), ...) {
  .cstr_options("clock_duration", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct clock_duration
.cstr_construct.clock_duration <- function(x, ...) {
  opts <- list(...)$opts$clock_duration %||% opts_clock_duration()
  if (is_corrupted_clock_duration(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.clock_duration", structure(NA, class = opts$constructor))
}

is_corrupted_clock_duration <- function(x) {
  is_corrupted_clock_int64(x)
}

#' @export
#' @method .cstr_construct.clock_duration duration
.cstr_construct.clock_duration.duration <- function(x, ...) {
  # dispatch to clock's method is intended
  n <- base::as.double(x)
  # the duration_*() helpers only accept integers
  if (any(abs(n) > .Machine$integer.max, na.rm = TRUE)) {
    return(.cstr_construct.clock_duration.list(x, ...))
  }
  precision <- clock_precisions[[attr(x, "precision") + 1]]
  fun <- sprintf("clock::duration_%ss", precision)
  code <- .cstr_apply(list(n), fun, ...)
  repair_attributes_clock_duration(x, code, ...)
}

#' @export
#' @method .cstr_construct.clock_duration list
.cstr_construct.clock_duration.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_clock_duration <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("clock_duration", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
    ignore = "precision"
  )
}

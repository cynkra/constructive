#' Constructive options for class 'clock_zoned_time'
#'
#' These options will be used on objects of class 'clock_zoned_time'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as_zoned_time"` (default): We build the object using `clock::as_zoned_time()`
#'   on the local naive time, providing the `zone` and, when some local times are
#'   ambiguous, the `ambiguous` argument.
#' * `"next"` : Use the constructor for the next supported class.
#' * `"list"` : We define as a list and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_clock_zoned_time>
#' @export
opts_clock_zoned_time <- function(constructor = c("as_zoned_time", "next", "list"), ...) {
  .cstr_options("clock_zoned_time", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct clock_zoned_time
.cstr_construct.clock_zoned_time <- function(x, ...) {
  opts <- list(...)$opts$clock_zoned_time %||% opts_clock_zoned_time()
  if (is_corrupted_clock_zoned_time(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.clock_zoned_time", structure(NA, class = opts$constructor))
}

is_corrupted_clock_zoned_time <- function(x) {
  # zoned times have a precision of at least "second"
  zone <- attr(x, "zone")
  is_corrupted_clock_int64(x, precision = 7:10) ||
    !is.character(zone) || length(zone) != 1 || is.na(zone)
}

#' @export
#' @method .cstr_construct.clock_zoned_time as_zoned_time
.cstr_construct.clock_zoned_time.as_zoned_time <- function(x, ...) {
  naive <- clock::as_naive_time(x)
  zone <- attr(x, "zone")
  earliest <- clock::as_zoned_time(naive, zone, ambiguous = "earliest")
  latest <- clock::as_zoned_time(naive, zone, ambiguous = "latest")
  args <- list(naive, zone = zone)
  is_ambiguous <- !vctrs::vec_equal(earliest, latest, na_equal = TRUE)
  if (any(is_ambiguous)) {
    ambiguous <- ifelse(vctrs::vec_equal(x, earliest, na_equal = TRUE), "earliest", "latest")
    args$ambiguous <- unique(ambiguous[is_ambiguous])
    if (length(args$ambiguous) > 1) args$ambiguous <- ambiguous
  }
  code <- .cstr_apply(args, "clock::as_zoned_time", ...)
  repair_attributes_clock_zoned_time(x, code, ...)
}

#' @export
#' @method .cstr_construct.clock_zoned_time list
.cstr_construct.clock_zoned_time.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_clock_zoned_time <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("clock_zoned_time", "clock_rcrd", "vctrs_rcrd", "vctrs_vctr"),
    ignore = c("precision", "zone")
  )
}

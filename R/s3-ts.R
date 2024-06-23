#' Constructive options for time-series objets
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"ts"` : We use `ts()`
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `"atomic"`
#' * `"atomic"` : We define as an atomic vector and repair attributes
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_ts>
#' @export
opts_ts  <- function(constructor = c("ts", "next", "atomic"), ...) {
  .cstr_options("ts", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ts
.cstr_construct.ts <- function(x, ...) {
  opts <- list(...)$opts$ts %||% opts_ts()
  if (is_corrupted_ts(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ts", structure(NA, class = opts$constructor))
}

is_corrupted_ts <- function(x) {
  # FIXME
  !typeof(x) %in% c("integer", "double")
}

#' @export
#' @method .cstr_construct.ts ts
.cstr_construct.ts.ts <- function(x, ...) {
  x_stripped <- x
  tsp <- attr(x, "tsp")
  attr(x_stripped, "tsp") <- NULL
  class(x_stripped) <- setdiff(oldClass(x), "ts")
  code <- .cstr_apply(list(x_stripped, frequency =  tail(tsp, 1), start = tsp[[1]]), "ts", ..., new_line = TRUE)
  repair_attributes_ts(x, code, ...)
}

#' @export
#' @method .cstr_construct.ts atomic
.cstr_construct.ts.atomic <- function(x, ...) {
  # ts can be integer or double
  .cstr_construct.default(x, ...)
}

repair_attributes_ts <- function(x, code, ..., pipe = NULL) {
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = "tsp",
    idiomatic_class = "ts"
  )
}

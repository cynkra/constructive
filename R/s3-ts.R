constructors$ts <- new.env()

#' Constructive options for time-series objets
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"ts"` : We use `ts()`
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `"atomic"`
#' * `"atomic"` : We define as an atomic vector and repair attributes
#'
#' @param constructor String. Name of the function used to construct the environment.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_ts  <- function(constructor = c("ts", "next", "atomic"), ...) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "ts"),
    ellipsis::check_dots_empty()
  )
  .cstr_options("ts", constructor = constructor)
}

#' @export
.cstr_construct.ts <- function(x, ...) {
  opts <- .cstr_fetch_opts("ts", ...)
  if (is_corrupted_ts(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$ts[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_ts <- function(x) {
  # TODO
  FALSE
}

constructors$ts$ts <- function(x, ...) {
  x_stripped <- x
  tsp <- attr(x, "tsp")
  attr(x_stripped, "tsp") <- NULL
  class(x_stripped) <- setdiff(oldClass(x), "ts")
  code <- .cstr_apply(list(x_stripped, frequency =  tail(tsp, 1), start = tsp[[1]]), "ts", ..., new_line = TRUE)
  repair_attributes_ts(x, code, ...)
}

constructors$ts$atomic <- function(x, ...) {
  .cstr_construct.atomic(x, ...)
}

repair_attributes_ts <- function(x, code, ..., pipe = "base") {
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = "tsp",
    idiomatic_class = "ts"
  )
}

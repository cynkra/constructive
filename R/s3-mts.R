constructors$mts <- new.env()

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
opts_mts  <- function(constructor = c("ts", "next", "atomic"), ...) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "mts"),
    ellipsis::check_dots_empty()
  )
  .cstr_options("mts", constructor = constructor)
}

#' @export
.cstr_construct.mts <- function(x, ...) {
  opts <- .cstr_fetch_opts("mts", ...)
  if (is_corrupted_mts(x) || opts$constructor == "next") return(NextMethod())
  constructors$mts[[opts$constructor]](x, ...)
}

is_corrupted_mts <- function(x) {
  # TODO
  FALSE
}

constructors$mts$ts <- function(x, ...) {
  x_stripped <- x
  tsp <- attr(x, "tsp")
  attr(x_stripped, "tsp") <- NULL
  class(x_stripped) <- setdiff(oldClass(x), c("mts", "ts"))
  .cstr_apply(list(x_stripped, frequency =  tail(tsp, 1), start = tsp[[1]]), "ts", ..., new_line = TRUE)
}

constructors$mts$atomic <- function(x, ...) {
  .cstr_construct.atomic(x, ...)
}

repair_attributes_mts <- function(x, code, ..., pipe = NULL) {
  nms <- colnames(x) %||% paste("Series", seq_len(ncol(x)))
  if (identical(attr(x, "dimnames")[[2]], nms)) attr(x, "dimnames") <- NULL
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = c("tsp", "dim"),
    idiomatic_class = c("mts", "ts", "matrix")
  )
}

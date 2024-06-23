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
#' @return An object of class <constructive_options/constructive_options_mts>
#' @export
opts_mts  <- function(constructor = c("ts", "next", "atomic"), ...) {
  .cstr_options("mts", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct mts
.cstr_construct.mts <- function(x, ...) {
  opts <- list(...)$opts$mts %||% opts_mts()
  if (is_corrupted_mts(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.mts", structure(NA, class = opts$constructor))
}

is_corrupted_mts <- function(x) {
  # TODO
  !typeof(x) %in% c("integer", "double")
}

#' @export
#' @method .cstr_construct.mts ts
.cstr_construct.mts.ts <- function(x, ...) {
  x_stripped <- x
  tsp <- attr(x, "tsp")
  attr(x_stripped, "tsp") <- NULL
  class(x_stripped) <- setdiff(oldClass(x), c("mts", "ts"))
  .cstr_apply(list(x_stripped, frequency =  tail(tsp, 1), start = tsp[[1]]), "ts", ..., new_line = TRUE)
}

#' @export
#' @method .cstr_construct.mts atomic
.cstr_construct.mts.atomic <- function(x, ...) {
  .cstr_construct.default(x, ...)
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

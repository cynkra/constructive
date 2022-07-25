#' @export
construct_idiomatic.ts <- function(x, pipe, ...) {
  tsp <- attr(x, "tsp")
  attr(x, "tsp") <- NULL
  class(x) <- setdiff(oldClass(x), "ts")
  construct_apply(list(x, frequency =  tail(tsp, 1), start = tsp[[1]]), "ts", new_line = TRUE, ...)
}

#' @export
repair_attributes.ts <- function(x, code, pipe ="base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    ignore = "tsp",
    idiomatic_class = "ts",
    ...
  )
}

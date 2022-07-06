#' @export
construct_idiomatic.mts <- function(x, pipe, ...) {
  tsp <- attr(x, "tsp")
  attr(x, "tsp") <- NULL
  class(x) <- setdiff(oldClass(x), c("mts", "ts"))
  #browser()
  construct_apply(list(x, frequency =  tail(tsp, 1), start = tsp[[1]]), "ts", new_line = TRUE)
}

#' @export
repair_attributes.mts <- function(x, code, pipe ="base", ...) {
  nms <- colnames(x) %||% paste("Series", seq(ncol(x)))
  if (identical(attr(x, "dimnames")[[2]], nms)) attr(x, "dimnames") <- NULL
  repair_attributes_impl(
    x, code, pipe,
    ignore = c("tsp", "dim"),
    idiomatic_class = c("mts", "ts", "matrix"),
    ...
  )
}

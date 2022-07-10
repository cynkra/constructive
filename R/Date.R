#' @export
construct_idiomatic.Date <- function(x, ...) {
  construct_apply(list(format(x)), "as.Date", new_line = FALSE, ...)
}

#' @export
repair_attributes.Date <- function(x, code, pipe = "base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    idiomatic_class = "Date",
    ...
  )
}

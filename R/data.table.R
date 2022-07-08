#' @export
construct_idiomatic.data.table <- function(x, keep_trailing_comma, ...) {
  construct_apply(x, fun = "data.table::data.table", keep_trailing_comma = FALSE, ...)
}

#' @export
repair_attributes.data.table <- function(x, code, pipe = "base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    ignore = c("row.names", ".internal.selfref"),
    idiomatic_class = c("data.table", "data.frame"),
    ...
  )
}

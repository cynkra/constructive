#' @export
construct_idiomatic.data.table <- function(x, ...) {
  construct_apply(x, fun = "data.table::data.table", ...)
}

#' @export
repair_attributes.data.table <- function(x, code, ..., pipe = "base") {
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = c("row.names", ".internal.selfref"),
    idiomatic_class = c("data.table", "data.frame")
  )
}

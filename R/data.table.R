#' @export
construct_idiomatic.data.table <- function(x, ...) {
  construct_apply(x, fun = "data.table::data.table", ...)
}

#' @export
repair_attributes.data.table <- function(x, code, ..., pipe = "base") {
  ignore <- c("row.names", ".internal.selfref")
  if (identical(names(x), character())) ignore <- c(ignore, "names")
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = ignore,
    idiomatic_class = c("data.table", "data.frame")
  )
}

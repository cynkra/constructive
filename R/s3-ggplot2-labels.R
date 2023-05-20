#' @export
.cstr_construct.labels <- function(x, ...) {
  code <- .cstr_apply(x, fun = "ggplot2::labs", ...)
  repair_attributes.labels(x, code, ...)
}

repair_attributes.labels <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = "labels", ...)
}

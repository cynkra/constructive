#' @export
construct_idiomatic.labels <- function(x, ...) {
  construct_apply(x, fun = "ggplot2::labs", ...)
}

repair_attributes.labels <- function(x, ...) {
  repair_attributes_impl(x, idiomatic_class = "labels", ...)
}

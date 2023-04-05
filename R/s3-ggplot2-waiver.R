#' @export
construct_idiomatic.waiver <- function(x, ...) {
  "ggplot2::waiver()"
}

repair_attributes.waiver <- function(x, ...) {
  repair_attributes_impl(x, idiomatic_class = "waiver", ...)
}

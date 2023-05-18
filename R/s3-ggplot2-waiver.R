#' @export
.cstr_construct.waiver <- function(x, ...) {
  code <- "ggplot2::waiver()"
  repair_attributes.waiver(x, code, ...)
}

repair_attributes.waiver <- function(x, ...) {
  repair_attributes_impl(x, idiomatic_class = "waiver", ...)
}

#' @export
.cstr_construct.waiver <- function(x, ...) {
  code <- "ggplot2::waiver()"
  repair_attributes_waiver(x, code, ...)
}

repair_attributes_waiver <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = "waiver", ...)
}

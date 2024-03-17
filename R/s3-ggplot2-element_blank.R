constructors$element_blank <- new.env()

#' @export
#' @rdname other-opts
opts_element_blank <- new_constructive_opts_function("element_blank", "element_blank")

#' @export
.cstr_construct.element_blank <- new_constructive_method("element_blank", "element_blank")

is_corrupted_element_blank <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$element_blank$element_blank <- function(x, ...) {
  code <- "ggplot2::element_blank()"
  repair_attributes_element_blank(x, code, ...)
}

repair_attributes_element_blank <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_blank", "element"), ...)
}

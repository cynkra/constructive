constructors$element_blank <- new.env()

#' @export
#' @rdname other-opts
opts_element_blank <- new_constructive_opts_function("element_blank", c("element_blank", "next", "list"))

#' @export
.cstr_construct.element_blank <- new_constructive_method("element_blank", c("element_blank", "next", "list"))

is_corrupted_element_blank <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$element_blank$list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
constructors$element_blank$element_blank <- function(x, ...) {
  code <- "ggplot2::element_blank()"
  repair_attributes_element_blank(x, code, ...)
}

repair_attributes_element_blank <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_blank", "element"), ...)
}

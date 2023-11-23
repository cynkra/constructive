constructors$element_rect <- new.env()

#' @export
opts_element_rect <- new_constructive_opts_function("element_rect", "element_rect")

#' @export
.cstr_construct.element_rect <- new_constructive_method("element_rect", "element_rect")

is_corrupted_element_rect <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$element_rect$element_rect <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_rect)
  code <- .cstr_apply(args, "ggplot2::element_rect", ...)
  repair_attributes_element_rect(x, code, ...)
}

repair_attributes_element_rect <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_rect", "element"), ...)
}

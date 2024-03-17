constructors$element_rect <- new.env()

#' @export
#' @rdname other-opts
opts_element_rect <- new_constructive_opts_function("element_rect", c("element_rect", "next", "list"))

#' @export
.cstr_construct.element_rect <- new_constructive_method("element_rect", c("element_rect", "next", "list"))

is_corrupted_element_rect <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$element_rect$list <- function(x, ...) {
  .cstr_construct.list(x, ...)
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

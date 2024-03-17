constructors$element_render <- new.env()

#' @export
#' @rdname other-opts
opts_element_render <- new_constructive_opts_function("element_render", c("element_render", "next", "list"))

#' @export
.cstr_construct.element_render <- new_constructive_method("element_render", c("element_render", "next", "list"))

is_corrupted_element_render <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$element_render$list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
constructors$element_render$element_render <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_render)
  code <- .cstr_apply(args, "ggplot2::element_render", ...)
  repair_attributes_element_render(x, code, ...)
}

repair_attributes_element_render <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_render", "element"), ...)
}

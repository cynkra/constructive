constructors$element_grob <- new.env()

#' @export
#' @rdname other-opts
opts_element_grob <- new_constructive_opts_function("element_grob", "element_grob")

#' @export
.cstr_construct.element_grob <- new_constructive_method("element_grob", "element_grob")

is_corrupted_element_grob <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$element_grob$element_grob <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_grob)
  code <- .cstr_apply(args, "ggplot2::element_grob", ...)
  repair_attributes_element_grob(x, code, ...)
}

repair_attributes_element_grob <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_grob", "element"), ...)
}

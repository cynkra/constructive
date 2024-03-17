constructors$element_line <- new.env()

#' @export
#' @rdname other-opts
opts_element_line <- new_constructive_opts_function("element_line", c("element_line", "next", "list"))

#' @export
.cstr_construct.element_line <- new_constructive_method("element_line", c("element_line", "next", "list"))

is_corrupted_element_line <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$element_line$list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
constructors$element_line$element_line <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_line)
  code <- .cstr_apply(args, "ggplot2::element_line", ...)
  repair_attributes_element_line(x, code, ...)
}

repair_attributes_element_line <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_line", "element"), ...)
}

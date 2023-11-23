constructors$element_text <- new.env()

#' @export
opts_element_text <- new_constructive_opts_function("element_text", "element_text")

#' @export
.cstr_construct.element_text <- new_constructive_method("element_text", "element_text")

is_corrupted_element_text <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$element_text$element_text <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_text)
  code <- .cstr_apply(args, "ggplot2::element_text", ...)
  repair_attributes_element_text(x, code, ...)
}

repair_attributes_element_text <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_text", "element"), ...)
}

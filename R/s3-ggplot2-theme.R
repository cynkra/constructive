#' @export
construct_idiomatic.theme <- function(x, ...) {
  args <- unclass(x)
  args$complete <- attr(x, "complete")
  args$validate <- attr(x, "validate")
  construct_apply(args, "ggplot2::theme", ...)
}

#' @export
repair_attributes.theme <- function(x, ...) {
  repair_attributes_impl(x, idiomatic_class = c("theme", "gg"), ignore = c("complete", "validate"), ...)
}

#' @export
construct_idiomatic.element_blank <- function(x, ...) {
  "ggplot2::element_blank()"
}

#' @export
repair_attributes.element_blank <- function(x, ...) {
  repair_attributes_impl(x, idiomatic_class = c("element_blank", "element"), ...)
}

#' @export
construct_idiomatic.element_grob <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_grob)
  construct_apply(args, "ggplot2::element_grob", ...)
}

#' @export
repair_attributes.element_grob <- function(x, ...) {
  repair_attributes_impl(x, idiomatic_class = c("element_grob", "element"), ...)
}

#' @export
construct_idiomatic.element_rect <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_rect)
  construct_apply(args, "ggplot2::element_rect", ...)
}

#' @export
repair_attributes.element_rect <- function(x, ...) {
  repair_attributes_impl(x, idiomatic_class = c("element_rect", "element"), ...)
}

#' @export
construct_idiomatic.element_render <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_render)
  construct_apply(args, "ggplot2::element_render", ...)
}

#' @export
repair_attributes.element_render <- function(x, ...) {
  repair_attributes_impl(x, idiomatic_class = c("element_render", "element"), ...)
}

#' @export
construct_idiomatic.element_text <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_text)
  construct_apply(args, "ggplot2::element_text", ...)
}

#' @export
repair_attributes.element_text <- function(x, ...) {
  repair_attributes_impl(x, idiomatic_class = c("element_text", "element"), ...)
}

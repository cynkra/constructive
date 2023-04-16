#' @export
construct_idiomatic.theme <- function(x, ...) {
  args <- unclass(x)
  args$complete <- if (attr(x, "complete")) TRUE
  args$validate <- if (!attr(x, "validate")) FALSE
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

#' @export
construct_idiomatic.element_line <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_line)
  construct_apply(args, "ggplot2::element_line", ...)
}

#' @export
repair_attributes.element_line <- function(x, ...) {
  repair_attributes_impl(x, idiomatic_class = c("element_line", "element"), ...)
}

#' @export
construct_idiomatic.simpleUnit <- function(x, ...) {
  lkp <- c(
    npc = 0L, cm = 1L, inches = 2L, mm = 7L, points = 8L, picas = 9L,
    bigpts = 10L, dida = 11L, cicero = 12L, scaledpts = 13L, lines = 3L,
    char = 18L, native = 4L, snpc = 6L, mylines = 103L, mychar = 104L,
    mystrwidth = 105L, mystrheight = 106L
  )
  units <- names(lkp)[match(attr(x, "unit"), lkp)]
  x <- as.vector(x)
  construct_apply(list(x, units = units), "grid::unit", ...)
}

#' @export
repair_attributes.simpleUnit <- function(x, ...) {
  repair_attributes_impl(x, idiomatic_class = c("simpleUnit", "unit", "unit_v2"), ignore = "unit", ...)
}

#' @export
construct_idiomatic.margin <- function(x, ...) {
  lkp <- c(
    npc = 0L, cm = 1L, inches = 2L, mm = 7L, points = 8L, picas = 9L,
    bigpts = 10L, dida = 11L, cicero = 12L, scaledpts = 13L, lines = 3L,
    char = 18L, native = 4L, snpc = 6L, mylines = 103L, mychar = 104L,
    mystrwidth = 105L, mystrheight = 106L
  )
  unit <- names(lkp)[match(attr(x, "unit"), lkp)]
  x <- as.vector(x)
  construct_apply(list(t = x[1], r = x[2], b = x[3], l = x[4], unit = unit), "ggplot2::margin", ...)
}

#' @export
repair_attributes.margin <- function(x, ...) {
  repair_attributes_impl(x, idiomatic_class = c("margin", "simpleUnit", "unit", "unit_v2"), ignore = "unit", ...)
}

#' @export
construct_idiomatic.rel <- function(x, ...) {
  construct_apply(list(unclass(x)), "ggplot2::rel", ...)
}

#' @export
repair_attributes.rel <- function(x, ...) {
  repair_attributes_impl(x, idiomatic_class = "rel", ...)
}

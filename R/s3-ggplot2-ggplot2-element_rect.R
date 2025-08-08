#' @export
#' @rdname other-opts
opts_ggplot2_element_rect <- function(constructor = c("element_rect", "next"), ...) {
  .cstr_options("element_rect", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ggplot2::element_rect
`.cstr_construct.ggplot2::element_rect` <- function(x, ...) {
  opts <- list(...)$opts$element_rect %||% opts_ggplot2_element_rect()
  if (`is_corrupted_ggplot2::element_rect`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ggplot2::element_rect", structure(NA, class = opts$constructor))
}

`is_corrupted_ggplot2::element_rect` <- function(x) {
  typeof(x) != "object"
}

#' @export
#' @method .cstr_construct.ggplot2::element_rect element_rect
`.cstr_construct.ggplot2::element_rect.element_rect` <- function(x, ...) {
  args <- list(
    fill = x@fill,
    colour = x@colour,
    linewidth = x@linewidth,
    linetype = x@linetype,
    linejoin = x@linejoin,
    inherit.blank = attr(x, "inherit.blank")
  )
  args <- keep_only_non_defaults(args, ggplot2::element_rect)
  code <- .cstr_apply(args, "ggplot2::element_rect", ...)
  `repair_attributes_ggplot2::element_rect`(x, code, ...)
}

`repair_attributes_ggplot2::element_rect` <- function(x, ...) {
  property_names <- names(attr(attr(x, "S7_class"),"properties"))
  .cstr_repair_attributes(
    x,
    idiomatic_class = c(
      "ggplot2::element_rect",
      "element_rect",
      "ggplot2::element",
      "S7_object",
      "element"
    ),
    ignore = c("S7_class", property_names),
    ...
  )
}

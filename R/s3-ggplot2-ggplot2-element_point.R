#' @export
#' @rdname other-opts
opts_ggplot2_element_point <- function(constructor = c("element_point", "next"), ...) {
  .cstr_options("element_point", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ggplot2::element_point
`.cstr_construct.ggplot2::element_point` <- function(x, ...) {
  opts <- list(...)$opts$element_point %||% opts_ggplot2_element_point()
  if (`is_corrupted_ggplot2::element_point`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ggplot2::element_point", structure(NA, class = opts$constructor))
}

`is_corrupted_ggplot2::element_point` <- function(x) {
  typeof(x) != "object"
}

#' @export
#' @method .cstr_construct.ggplot2::element_point element_point
`.cstr_construct.ggplot2::element_point.element_point` <- function(x, ...) {
  args <- list(
    colour = x@colour,
    shape = x@shape,
    size = x@size,
    fill = x@fill,
    stroke = x@stroke,
    inherit.blank = x@inherit.blank
  )
  args <- keep_only_non_defaults(args, getFromNamespace("element_point", "ggplot2"))
  code <- .cstr_apply(args, "ggplot2::element_point", ...)
  `repair_attributes_ggplot2::element_point`(x, code, ...)
}

`repair_attributes_ggplot2::element_point` <- function(x, ...) {
  property_names <- names(attr(attr(x, "S7_class"),"properties"))
  .cstr_repair_attributes(
    x,
    idiomatic_class = c(
      "ggplot2::element_point",
      "ggplot2::element",
      "S7_object"
    ),
    ignore = c("S7_class", property_names),
    ...
  )
}

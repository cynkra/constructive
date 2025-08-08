#' @export
#' @rdname other-opts
opts_ggplot2_element_polygon <- function(constructor = c("element_polygon", "next"), ...) {
  .cstr_options("element_polygon", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ggplot2::element_polygon
`.cstr_construct.ggplot2::element_polygon` <- function(x, ...) {
  opts <- list(...)$opts$element_polygon %||% opts_ggplot2_element_polygon()
  if (`is_corrupted_ggplot2::element_polygon`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ggplot2::element_polygon", structure(NA, class = opts$constructor))
}

`is_corrupted_ggplot2::element_polygon` <- function(x) {
  typeof(x) != "object"
}

#' @export
#' @method .cstr_construct.ggplot2::element_polygon element_polygon
`.cstr_construct.ggplot2::element_polygon.element_polygon` <- function(x, ...) {
  args <- list(
    fill = x@fill,
    colour = x@colour,
    linewidth = x@linewidth,
    linetype = x@linetype,
    linejoin = x@linejoin,
    inherit.blank = x@inherit.blank
  )
  args <- keep_only_non_defaults(args,  getFromNamespace("element_polygon", "ggplot2"))
  code <- .cstr_apply(args, "ggplot2::element_polygon", ...)
  `repair_attributes_ggplot2::element_polygon`(x, code, ...)
}

`repair_attributes_ggplot2::element_polygon` <- function(x, ...) {
  property_names <- names(attr(attr(x, "S7_class"),"properties"))
  .cstr_repair_attributes(
    x,
    idiomatic_class = c(
      "ggplot2::element_polygon",
      "ggplot2::element",
      "S7_object"
    ),
    ignore = c("S7_class", property_names),
    ...
  )
}

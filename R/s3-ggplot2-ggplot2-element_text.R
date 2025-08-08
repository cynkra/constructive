#' @export
#' @rdname other-opts
opts_ggplot2_element_text <- function(constructor = c("element_text", "next"), ...) {
  .cstr_options("element_text", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ggplot2::element_text
`.cstr_construct.ggplot2::element_text` <- function(x, ...) {
  opts <- list(...)$opts$element_text %||% opts_ggplot2_element_text()
  if (`is_corrupted_ggplot2::element_text`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ggplot2::element_text", structure(NA, class = opts$constructor))
}

`is_corrupted_ggplot2::element_text` <- function(x) {
  typeof(x) != "object"
}

#' @export
#' @method .cstr_construct.ggplot2::element_text element_text
`.cstr_construct.ggplot2::element_text.element_text` <- function(x, ...) {
  args <- list(
    family = x@family,
    face = x@face,
    colour = x@colour,
    size = x@size,
    hjust = x@hjust,
    vjust = x@vjust,
    angle = x@angle,
    lineheight = x@lineheight,
    margin = x@margin,
    debug = x@debug,
    inherit.blank = x@inherit.blank
  )
  args <- keep_only_non_defaults(args, ggplot2::element_text)
  code <- .cstr_apply(args, "ggplot2::element_text", ...)
  `repair_attributes_ggplot2::element_text`(x, code, ...)
}

`repair_attributes_ggplot2::element_text` <- function(x, ...) {
  property_names <- names(attr(attr(x, "S7_class"),"properties"))
  .cstr_repair_attributes(
    x,
    idiomatic_class = c(
      "ggplot2::element_text",
      "element_text",
      "ggplot2::element",
      "S7_object",
      "element"
    ),
    ignore = c("S7_class", property_names),
    ...
  )
}

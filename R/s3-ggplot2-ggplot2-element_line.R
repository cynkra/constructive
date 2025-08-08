#' @export
#' @rdname other-opts
opts_ggplot2_element_line <- function(constructor = c("element_line", "next"), ...) {
  .cstr_options("element_line", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ggplot2::element_line
`.cstr_construct.ggplot2::element_line` <- function(x, ...) {
  opts <- list(...)$opts$element_line %||% opts_ggplot2_element_line()
  if (`is_corrupted_ggplot2::element_line`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ggplot2::element_line", structure(NA, class = opts$constructor))
}

`is_corrupted_ggplot2::element_line` <- function(x) {
  typeof(x) != "object"
}

#' @export
#' @method .cstr_construct.ggplot2::element_line element_line
`.cstr_construct.ggplot2::element_line.element_line` <- function(x, ...) {
  args <- list(
    colour = x@colour,
    linewidth = x@linewidth,
    linetype = x@linetype,
    lineend = x@lineend,
    linejoin = x@linejoin,
    arrow = attr(x, "arrow"),
    arrow.fill = x@arrow.fill,
    inherit.blank = attr(x, "inherit.blank")
  )
  # deal with hidden defaults
  if (isFALSE(args$arrow)) args$arrow <- NULL
  if (identical(args$arrow.fill, args$colour)) args$arrow.fill <- NULL
  args <- keep_only_non_defaults(args, ggplot2::element_line)
  code <- .cstr_apply(args, "ggplot2::element_line", ...)
  `repair_attributes_ggplot2::element_line`(x, code, ...)
}

`repair_attributes_ggplot2::element_line` <- function(x, ...) {
  property_names <- names(attr(attr(x, "S7_class"),"properties"))
  .cstr_repair_attributes(
    x,
    idiomatic_class = c(
      "ggplot2::element_line",
      "element_line",
      "ggplot2::element",
      "S7_object",
      "element"
    ),
    ignore = c("S7_class", property_names),
    ...
  )
}

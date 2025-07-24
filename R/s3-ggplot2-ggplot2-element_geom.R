#' @export
#' @rdname other-opts
`opts_ggplot2::element_geom` <- function(constructor = c("element_geom", "next"), ...) {
  .cstr_options("element_geom", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ggplot2::element_geom
`.cstr_construct.ggplot2::element_geom` <- function(x, ...) {
  opts <- list(...)$opts$element_geom %||% `opts_ggplot2::element_geom`()
  if (`is_corrupted_ggplot2::element_geom`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ggplot2::element_geom", structure(NA, class = opts$constructor))
}

`is_corrupted_ggplot2::element_geom` <- function(x) {
  typeof(x) != "object"
}

#' @export
#' @method .cstr_construct.ggplot2::element_geom element_geom
`.cstr_construct.ggplot2::element_geom.element_geom` <- function(x, ...) {
  args <- list(
    ink = x@ink,
    paper = x@paper,
    accent = x@accent,
    linewidth = x@linewidth,
    borderwidth = x@borderwidth,
    linetype = x@linetype,
    bordertype = x@bordertype,
    family = x@family,
    fontsize = x@fontsize,
    pointsize = x@pointsize,
    pointshape = x@pointshape,
    colour = x@colour,
    fill = x@fill
  )
  args <- keep_only_non_defaults(args, ggplot2::element_geom)
  code <- .cstr_apply(args, "ggplot2::element_geom", ...)
  `repair_attributes_ggplot2::element_geom`(x, code, ...)
}

`repair_attributes_ggplot2::element_geom` <- function(x, ...) {
  property_names <- names(attr(attr(x, "S7_class"),"properties"))
  .cstr_repair_attributes(
    x,
    idiomatic_class = c(
      "ggplot2::element_geom",
      "ggplot2::element",
      "S7_object"
    ),
    ignore = c("S7_class", property_names),
    ...
  )
}

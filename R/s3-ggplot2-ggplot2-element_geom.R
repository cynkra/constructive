#' @export
#' @rdname other-opts
opts_ggplot2_element_geom <- function(constructor = c("element_geom", "next"), ...) {
  .cstr_options("element_geom", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ggplot2::element_geom
`.cstr_construct.ggplot2::element_geom` <- function(x, ...) {
  opts <- list(...)$opts$element_geom %||% opts_ggplot2_element_geom()
  if (`is_corrupted_ggplot2::element_geom`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ggplot2::element_geom", structure(NA, class = opts$constructor))
}

`is_corrupted_ggplot2::element_geom` <- function(x) {
  typeof(x) != "object"
}

#' @export
#' @method .cstr_construct.ggplot2::element_geom element_geom
`.cstr_construct.ggplot2::element_geom.element_geom` <- function(x, ...) {
  # points per millimeter

  # a factor .pt meaning points per millimeter is applied
  # .pt = points per inch / mm per inch = 72.27 / 25.4
  .pt <-  72.27/25.4
  fontsize_reconstructed <- x@fontsize * 72.27 / 25.4

  # because of floating point there are several solutions
  # we assume the user provided a round amount so we check if the
  # round version works and take it if it does
  fontsize_rounded <- round(fontsize_reconstructed, 14)
  if (identical(fontsize_rounded * 25.4 / 72.27, fontsize_reconstructed * 25.4 / 72.27)) {
    fontsize_reconstructed <- fontsize_rounded
  }

  args <- list(
    ink = x@ink,
    paper = x@paper,
    accent = x@accent,
    linewidth = x@linewidth,
    borderwidth = x@borderwidth,
    linetype = x@linetype,
    bordertype = x@bordertype,
    family = x@family,
    # a factor .pt meaning points per millimeter is applies
    # .pt = points per inch / mm per inch = 72.27 / 25.4
    fontsize = fontsize_rounded,
    pointsize = x@pointsize,
    pointshape = x@pointshape,
    colour = x@colour,
    fill = x@fill
  )
  args <- keep_only_non_defaults(args, getFromNamespace("element_geom", "ggplot2"))
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

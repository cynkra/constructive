#' @export
#' @rdname other-opts
opts_CoordSf <- function(constructor = c("coord_sf", "next", "environment"), ...) {
  .cstr_options("CoordSf", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct CoordSf
.cstr_construct.CoordSf <- function(x, ...) {
  opts <- list(...)$opts$CoordSf %||% opts_CoordSf()
  if (is_corrupted_CoordSf(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.CoordSf", structure(NA, class = opts$constructor))
}

is_corrupted_CoordSf <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.CoordSf environment
.cstr_construct.CoordSf.environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
#' @method .cstr_construct.CoordSf coord_sf
.cstr_construct.CoordSf.coord_sf <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    expand = x$expand,
    crs = x$crs,
    default_crs = x$default_crs,
    datum = x$datum,
    label_graticule = if (length(x$label_graticule)) x$label_graticule else ggplot2::waiver(),
    label_axes = paste(x$label_axes, collapse = ""),
    lims_method = x$lims_method,
    ndiscr = x$ndiscr,
    #default = x$default, # triggers message on definition, but no effect on what's printed
    clip = x$clip
  )
  # handle hidden default
  if (args$label_axes == "--EN") args$label_axes <- NULL
  args <- keep_only_non_defaults(args, ggplot2::coord_sf)
  .cstr_apply(args, "ggplot2::coord_sf", ...)
}


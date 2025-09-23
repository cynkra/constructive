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
  if (!is.environment(x)) return(TRUE)
  nms <- c(
    "expand", "clip", "limits", "ndiscr", "lims_method", "reverse", "default_crs",
    "super", "default", "label_axes", "crs", "label_graticule", "datum"
  )
  if (!all(nms %in% names(x))) return(TRUE)
  if (!is.list(x$limits) || !all(c("x", "y") %in% names(x$limits))) return(TRUE)
  if (!rlang::is_bool(x$expand)) return(TRUE)
  if (!is.null(x$crs) && !is.list(x$crs)) return(TRUE)
  if (!is.null(x$default_crs) && !is.list(x$default_crs)) return(TRUE)
  if (!is.null(x$datum) && !is.list(x$datum)) return(TRUE)
  if (!is.character(x$label_graticule) && is_corrupted_waiver(x$label_graticule)) return(TRUE)
  if (!is.list(x$label_axes)) return(TRUE)
  if (!rlang::is_string(x$lims_method)) return(TRUE)
  if (!is.numeric(x$ndiscr)) return(TRUE)
  if (!rlang::is_string(x$clip)) return(TRUE)
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


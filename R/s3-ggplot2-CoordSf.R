constructors$CoordSf <- new.env()

#' @export
#' @rdname other-opts
opts_CoordSf <- function(constructor = c("coord_sf", "next", "environment"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("CoordSf", constructor = constructor)
}

#' @export
.cstr_construct.CoordSf <- function(x, opts, ...) {
  opts_local <- opts$CoordSf %||% opts_CoordSf()
  if (is_corrupted_CoordSf(x) || opts_local$constructor == "next") return(NextMethod())
  constructor <- constructors$CoordSf[[opts_local$constructor]]
  constructor(x, opts = opts, ...)
}

is_corrupted_CoordSf <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$CoordSf$environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
constructors$CoordSf$coord_sf <- function(x, ...) {
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


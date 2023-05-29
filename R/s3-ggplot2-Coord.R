#' @export
.cstr_construct.CoordCartesian <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    expand = x$expand,
    #default = x$default, triggers message on definition, but no effect on what's printed
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_cartesian)
  .cstr_apply(args, "ggplot2::coord_cartesian", ...)
}

#' @export
.cstr_construct.CoordFixed <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    ratio = x$ratio,
    expand = x$expand,
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_fixed)
  .cstr_apply(args, "ggplot2::coord_fixed", ...)
}


#' @export
.cstr_construct.CoordFlip <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    expand = x$expand,
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_flip)
  .cstr_apply(args, "ggplot2::coord_flip", ...)
}


#' @export
.cstr_construct.CoordMap <- function(x, ...) {
  args <- c(
    list(projection = x$projection),
    if (length(x$params)) as.list(x$params) else NULL,
    list(
      orientation = x$orientation,
      xlim = x$limits$x,
      ylim = x$limits$y,
      clip = x$clip
    )
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_map)
  .cstr_apply(args, "ggplot2::coord_map", ...)
}


#' @export
.cstr_construct.CoordMunch <- function(x, ...) {
  # untested because didn't find any use case
  args <- list(
    coord = x$coordinates,
    data = x$data,
    range = x$range,
    segment_length = x$segment_length
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_munch)
  .cstr_apply(args, "ggplot2::coord_munch", ...)
}


#' @export
.cstr_construct.CoordPolar <- function(x, ...) {
  args <- list(
    theta = x$theta,
    start = x$start,
    direction = x$direction,
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_polar)
  .cstr_apply(args, "ggplot2::coord_polar", ...)
}

#' @export
.cstr_construct.CoordQuickmap <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    expand = x$expand,
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_quickmap)
  .cstr_apply(args, "ggplot2::coord_quickmap", ...)
}

#' @export
.cstr_construct.CoordSf <- function(x, ...) {
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

#' @export
.cstr_construct.CoordTrans <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    clip = x$clip,
    expand = x$expand
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_trans)
  args_chr <- lapply(args, .cstr_construct, ...)
  xy <- list(
    x = .cstr_apply(unclass(x$trans$x), "scales::trans_new", ...),
    y = .cstr_apply(unclass(x$trans$y), "scales::trans_new", ...)
  )
  .cstr_apply(c(args_chr, xy), "ggplot2::coord_trans", recurse = FALSE, ...)
}

#' @export
construct_idiomatic.CoordCartesian <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    expand = x$expand,
    #default = x$default, triggers message on definition, but no effect on what's printed
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_cartesian)
  construct_apply(args, "ggplot2::coord_cartesian", ...)
}

#' @export
construct_idiomatic.CoordFixed <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    ratio = x$ratio,
    expand = x$expand,
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_fixed)
  construct_apply(args, "ggplot2::coord_fixed", ...)
}


#' @export
construct_idiomatic.CoordFlip <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    expand = x$expand,
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_flip)
  construct_apply(args, "ggplot2::coord_flip", ...)
}


#' @export
construct_idiomatic.CoordMap <- function(x, ...) {
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
  construct_apply(args, "ggplot2::coord_map", ...)
}


#' @export
construct_idiomatic.CoordMunch <- function(x, ...) {
  # untested because didn't find any use case
  args <- list(
    coord = x$coord,
    data = x$data,
    range = x$range,
    segment_length = x$segment_length
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_munch)
  construct_apply(args, "ggplot2::coord_munch", ...)
}


#' @export
construct_idiomatic.CoordPolar <- function(x, ...) {
  args <- list(
    theta = x$theta,
    start = x$start,
    direction = x$direction,
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_polar)
  construct_apply(args, "ggplot2::coord_polar", ...)
}

#' @export
construct_idiomatic.CoordQuickmap <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    expand = x$expand,
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_quickmap)
  construct_apply(args, "ggplot2::coord_quickmap", ...)
}

#' @export
construct_idiomatic.CoordSf <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    expand = x$expand,
    crs = x$crs,
    default_crs = x$default_crs,
    datum = x$datum,
    label_graticule = if (length(x$label_graticule)) x$label_graticule else ggplot2::waiver(),
    label_axes = paste(x$label_axes, collapse =""),
    lims_method = x$lims_method,
    ndiscr = x$ndiscr,
    #default = x$default, # triggers message on definition, but no effect on what's printed
    clip = x$clip
  )
  # handle hidden default
  if (args$label_axes == "--EN") args$label_axes <- NULL
  args <- keep_only_non_defaults(args, ggplot2::coord_sf)
  construct_apply(args, "ggplot2::coord_sf", ...)
}

#' @export
construct_idiomatic.CoordTrans<- function(x, ...) {
  # FIXME: unfinished
  return(construct_idiomatic.environment(x, ...))

  ns <- asNamespace("scales")
  fun_nms <- ls(ns, pattern = "_trans$")
  trans_funs <- eval(substitute(lapply(FUNS, call), list(FUNS = fun_nms)), ns)
  match(x$trans$x, trans_funs)
  args <- list(
    x = x$x,
    y = x$y,
    xlim = x$limits$x,
    ylim = x$limits$y,
    #default = x$default, # triggers message on definition, but no effect on what's printed
    clip = x$clip,
    expand = x$expand
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_trans)
  construct_apply(args, "ggplot2::coord_trans", ...)
}



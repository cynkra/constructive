#' @export
#' @rdname other-opts
opts_CoordFlip <- function(constructor = c("coord_flip", "next", "environment"), ...) {
  .cstr_options("CoordFlip", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct CoordFlip
.cstr_construct.CoordFlip <- function(x, ...) {
  opts <- list(...)$opts$CoordFlip %||% opts_CoordFlip()
  if (is_corrupted_CoordFlip(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.CoordFlip", structure(NA, class = opts$constructor))
}

is_corrupted_CoordFlip <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.CoordFlip environment
.cstr_construct.CoordFlip.environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
#' @method .cstr_construct.CoordFlip coord_flip
.cstr_construct.CoordFlip.coord_flip <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    expand = x$expand,
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_flip)
  .cstr_apply(args, "ggplot2::coord_flip", ...)
}

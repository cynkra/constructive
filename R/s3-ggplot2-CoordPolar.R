#' @export
#' @rdname other-opts
opts_CoordPolar <- function(constructor = c("coord_polar", "next", "environment"), ...) {
  .cstr_options("CoordPolar", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct CoordPolar
.cstr_construct.CoordPolar <- function(x, ...) {
  opts <- list(...)$opts$CoordPolar %||% opts_CoordPolar()
  if (is_corrupted_CoordPolar(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.CoordPolar", structure(NA, class = opts$constructor))
}

is_corrupted_CoordPolar <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.CoordPolar environment
.cstr_construct.CoordPolar.environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
#' @method .cstr_construct.CoordPolar coord_polar
.cstr_construct.CoordPolar.coord_polar <- function(x, ...) {
  args <- list(
    theta = x$theta,
    start = x$start,
    direction = x$direction,
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_polar)
  .cstr_apply(args, "ggplot2::coord_polar", ...)
}

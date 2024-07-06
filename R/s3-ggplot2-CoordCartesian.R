#' @export
#' @rdname other-opts
opts_CoordCartesian <- function(constructor = c("coord_cartesian", "next", "environment"), ...) {
  .cstr_options("CoordCartesian", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct CoordCartesian
.cstr_construct.CoordCartesian <- function(x, ...) {
  opts <- list(...)$opts$CoordCartesian %||% opts_CoordCartesian()
  if (is_corrupted_CoordCartesian(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.CoordCartesian", structure(NA, class = opts$constructor))
}

is_corrupted_CoordCartesian <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.CoordCartesian environment
.cstr_construct.CoordCartesian.environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
#' @method .cstr_construct.CoordCartesian coord_cartesian
.cstr_construct.CoordCartesian.coord_cartesian <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    expand = x$expand,
    default = x$default,
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_cartesian)
  .cstr_apply(args, "ggplot2::coord_cartesian", ...)
}

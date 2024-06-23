#' @export
#' @rdname other-opts
opts_CoordQuickmap <- function(constructor = c("coord_quickmap", "next", "environment"), ...) {
  .cstr_options("CoordQuickmap", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct CoordQuickmap
.cstr_construct.CoordQuickmap <- function(x, ...) {
  opts <- list(...)$opts$CoordQuickmap %||% opts_CoordQuickmap()
  if (is_corrupted_CoordQuickmap(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.CoordQuickmap", structure(NA, class = opts$constructor))
}

is_corrupted_CoordQuickmap <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.CoordQuickmap environment
.cstr_construct.CoordQuickmap.environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
#' @method .cstr_construct.CoordQuickmap coord_quickmap
.cstr_construct.CoordQuickmap.coord_quickmap <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    expand = x$expand,
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_quickmap)
  .cstr_apply(args, "ggplot2::coord_quickmap", ...)
}

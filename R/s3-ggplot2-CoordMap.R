#' @export
#' @rdname other-opts
opts_CoordMap <- function(constructor = c("coord_map", "next", "environment"), ...) {
  .cstr_options("CoordMap", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct CoordMap
.cstr_construct.CoordMap <- function(x, ...) {
  opts <- list(...)$opts$CoordMap %||% opts_CoordMap()
  if (is_corrupted_CoordMap(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.CoordMap", structure(NA, class = opts$constructor))
}

is_corrupted_CoordMap <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.CoordMap environment
.cstr_construct.CoordMap.environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
#' @method .cstr_construct.CoordMap coord_map
.cstr_construct.CoordMap.coord_map <- function(x, ...) {
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

#' @export
#' @rdname other-opts
opts_CoordMunch <- function(constructor = c("coord_munch", "next", "environment"), ...) {
  .cstr_options("CoordMunch", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct CoordMunch
.cstr_construct.CoordMunch <- function(x, ...) {
  opts <- list(...)$opts$CoordMunch %||% opts_CoordMunch()
  if (is_corrupted_CoordMunch(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.CoordMunch", structure(NA, class = opts$constructor))
}

is_corrupted_CoordMunch <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.CoordMunch environment
.cstr_construct.CoordMunch.environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
#' @method .cstr_construct.CoordMunch coord_munch
.cstr_construct.CoordMunch.coord_munch <- function(x, ...) {
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

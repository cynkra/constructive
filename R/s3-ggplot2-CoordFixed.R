#' @export
#' @rdname other-opts
opts_CoordFixed <- function(constructor = c("coord_fixed", "next", "environment"), ...) {
  .cstr_options("CoordFixed", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct CoordFixed
.cstr_construct.CoordFixed <- function(x, ...) {
  opts <- list(...)$opts$CoordFixed %||% opts_CoordFixed()
  if (is_corrupted_CoordFixed(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.CoordFixed", structure(NA, class = opts$constructor))
}

is_corrupted_CoordFixed <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.CoordFixed environment
.cstr_construct.CoordFixed.environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
#' @method .cstr_construct.CoordFixed coord_fixed
.cstr_construct.CoordFixed.coord_fixed <- function(x, ...) {
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

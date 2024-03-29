constructors$CoordPolar <- new.env()

#' @export
#' @rdname other-opts
opts_CoordPolar <- function(constructor = c("coord_polar", "next", "environment"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("CoordPolar", constructor = constructor)
}

#' @export
.cstr_construct.CoordPolar <- function(x, ...) {
  opts <- .cstr_fetch_opts("CoordPolar", ...)
  if (is_corrupted_CoordPolar(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$CoordPolar[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_CoordPolar <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$CoordPolar$environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
constructors$CoordPolar$coord_polar <- function(x, ...) {
  args <- list(
    theta = x$theta,
    start = x$start,
    direction = x$direction,
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_polar)
  .cstr_apply(args, "ggplot2::coord_polar", ...)
}

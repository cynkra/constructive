constructors$CoordFlip <- new.env()

#' @export
opts_CoordFlip <- function(constructor = c("coord_flip", "next", "environment"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  .cstr_options("CoordFlip", constructor = constructor)
}

#' @export
.cstr_construct.CoordFlip <- function(x, ...) {
  opts <- .cstr_fetch_opts("CoordFlip", ...)
  if (is_corrupted_CoordFlip(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$CoordFlip[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_CoordFlip <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$CoordFlip$environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
constructors$CoordFlip$coord_flip <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    expand = x$expand,
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_flip)
  .cstr_apply(args, "ggplot2::coord_flip", ...)
}

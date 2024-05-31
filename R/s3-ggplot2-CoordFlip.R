constructors$CoordFlip <- new.env()

#' @export
#' @rdname other-opts
opts_CoordFlip <- function(constructor = c("coord_flip", "next", "environment"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("CoordFlip", constructor = constructor)
}

#' @export
.cstr_construct.CoordFlip <- function(x, opts = NULL, ...) {
  opts_local <- opts$CoordFlip %||% opts_CoordFlip()
  if (is_corrupted_CoordFlip(x) || opts_local[["constructor"]] == "next") return(NextMethod())
  constructor <- constructors$CoordFlip[[opts_local[["constructor"]]]]
  constructor(x, opts = opts, ...)
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

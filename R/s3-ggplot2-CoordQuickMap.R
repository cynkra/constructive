constructors$CoordQuickmap <- new.env()

#' @export
#' @rdname other-opts
opts_CoordQuickmap <- function(constructor = c("coord_quickmap", "next", "environment"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  .cstr_options("CoordQuickmap", constructor = constructor)
}

#' @export
.cstr_construct.CoordQuickmap <- function(x, ...) {
  opts <- .cstr_fetch_opts("CoordQuickmap", ...)
  if (is_corrupted_CoordQuickmap(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$CoordQuickmap[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_CoordQuickmap <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$CoordQuickmap$environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
constructors$CoordQuickmap$coord_quickmap <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    expand = x$expand,
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_quickmap)
  .cstr_apply(args, "ggplot2::coord_quickmap", ...)
}

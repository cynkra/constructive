constructors$CoordFixed <- new.env()

#' @export
#' @rdname other-opts
opts_CoordFixed <- function(constructor = c("coord_fixed", "next", "environment"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("CoordFixed", constructor = constructor)
}

#' @export
.cstr_construct.CoordFixed <- function(x, ...) {
  opts <- .cstr_fetch_opts("CoordFixed", ...)
  if (is_corrupted_CoordFixed(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$CoordFixed[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_CoordFixed <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$CoordFixed$environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
constructors$CoordFixed$coord_fixed <- function(x, ...) {
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

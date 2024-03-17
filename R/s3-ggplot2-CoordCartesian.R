constructors$CoordCartesian <- new.env()

#' @export
#' @rdname other-opts
opts_CoordCartesian <- function(constructor = c("coord_cartesian", "next", "environment"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("CoordCartesian", constructor = constructor)
}

#' @export
.cstr_construct.CoordCartesian <- function(x, ...) {
  opts <- .cstr_fetch_opts("CoordCartesian", ...)
  if (is_corrupted_CoordCartesian(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$CoordCartesian[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_CoordCartesian <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$CoordCartesian$environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
constructors$CoordCartesian$coord_cartesian <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    expand = x$expand,
    #default = x$default, triggers message on definition, but no effect on what's printed
    clip = x$clip
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_cartesian)
  .cstr_apply(args, "ggplot2::coord_cartesian", ...)
}

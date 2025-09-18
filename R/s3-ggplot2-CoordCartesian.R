#' @export
#' @rdname other-opts
opts_CoordCartesian <- function(constructor = c("default", "coord_cartesian", "next", "environment"), ...) {
  .cstr_options("CoordCartesian", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct CoordCartesian
.cstr_construct.CoordCartesian <- function(x, ...) {
  opts <- list(...)$opts$CoordCartesian %||% opts_CoordCartesian()
  if (is_corrupted_CoordCartesian(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.CoordCartesian", structure(NA, class = opts$constructor))
}

is_corrupted_CoordCartesian <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.CoordCartesian environment
.cstr_construct.CoordCartesian.environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}


#' @export
#' @method .cstr_construct.CoordCartesian coord_cartesian
.cstr_construct.CoordCartesian.default <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    expand = x$expand,
    default = x$default,
    clip = x$clip
  )
  # ratio arg and element was added after after v3.5.2 with the integration of
  # CoordFixed into CoordCartesian
  args$ratio <- x$ratio
  args <- keep_only_non_defaults(args, ggplot2::coord_cartesian)
  if (identical(names(args), "ratio")) {
     if (args$ratio == 1) {
      args <- list()
     }
    .cstr_apply(args, "ggplot2::coord_fixed", ...)
  } else {
    .cstr_apply(args, "ggplot2::coord_cartesian", ...)
  }
}

#' @export
#' @method .cstr_construct.CoordCartesian coord_cartesian
.cstr_construct.CoordCartesian.coord_cartesian <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    expand = x$expand,
    default = x$default,
    clip = x$clip
  )
  # ratio arg and element was added after after v3.5.2 with the integration of
  # CoordFixed into CoordCartesian
  args$ratio <- x$ratio
  args <- keep_only_non_defaults(args, ggplot2::coord_cartesian)
  .cstr_apply(args, "ggplot2::coord_cartesian", ...)
}

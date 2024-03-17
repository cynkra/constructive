constructors$CoordMap <- new.env()

#' @export
#' @rdname other-opts
opts_CoordMap <- function(constructor = c("coord_map", "next", "environment"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  .cstr_options("CoordMap", constructor = constructor)
}

#' @export
.cstr_construct.CoordMap <- function(x, ...) {
  opts <- .cstr_fetch_opts("CoordMap", ...)
  if (is_corrupted_CoordMap(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$CoordMap[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_CoordMap <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$CoordMap$environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
constructors$CoordMap$coord_map <- function(x, ...) {
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

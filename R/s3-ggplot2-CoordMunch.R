constructors$CoordMunch <- new.env()

#' @export
#' @rdname other-opts
opts_CoordMunch <- function(constructor = c("coord_munch", "next", "environment"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("CoordMunch", constructor = constructor)
}

#' @export
.cstr_construct.CoordMunch <- function(x, opts, ...) {
  opts_local <- opts$CoordMunch %||% opts_CoordMunch()
  if (is_corrupted_CoordMunch(x) || opts_local$constructor == "next") return(NextMethod())
  constructor <- constructors$CoordMunch[[opts_local$constructor]]
  constructor(x, opts = opts, ...)
}

is_corrupted_CoordMunch <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$CoordMunch$environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
constructors$CoordMunch$coord_munch <- function(x, ...) {
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

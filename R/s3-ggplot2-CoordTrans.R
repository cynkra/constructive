constructors$CoordTrans <- new.env()

#' @export
#' @rdname other-opts
opts_CoordTrans <- function(constructor = c("coord_trans", "next", "environment"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("CoordTrans", constructor = constructor)
}

#' @export
.cstr_construct.CoordTrans <- function(x, ...) {
  opts <- .cstr_fetch_opts("CoordTrans", ...)
  if (is_corrupted_CoordTrans(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$CoordTrans[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_CoordTrans <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$CoordTrans$environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
constructors$CoordTrans$coord_trans <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    clip = x$clip,
    expand = x$expand
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_trans)
  args_chr <- lapply(args, .cstr_construct, ...)
  xy <- list(
    x = .cstr_apply(unclass(x$trans$x), "scales::trans_new", ...),
    y = .cstr_apply(unclass(x$trans$y), "scales::trans_new", ...)
  )
  .cstr_apply(c(args_chr, xy), "ggplot2::coord_trans", recurse = FALSE, ...)
}

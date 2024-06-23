#' @export
#' @rdname other-opts
opts_CoordTrans <- function(constructor = c("coord_trans", "next", "environment"), ...) {
  .cstr_options("CoordTrans", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct CoordTrans
.cstr_construct.CoordTrans <- function(x, ...) {
  opts <- list(...)$opts$CoordTrans %||% opts_CoordTrans()
  if (is_corrupted_CoordTrans(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.CoordTrans", structure(NA, class = opts$constructor))
}

is_corrupted_CoordTrans <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.CoordTrans environment
.cstr_construct.CoordTrans.environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
#' @method .cstr_construct.CoordTrans coord_trans
.cstr_construct.CoordTrans.coord_trans <- function(x, ...) {
  args <- list(
    xlim = x$limits$x,
    ylim = x$limits$y,
    clip = x$clip,
    expand = x$expand
  )
  args <- keep_only_non_defaults(args, ggplot2::coord_trans)
  args_chr <- lapply(args, function(x, ...) .cstr_construct(x, ...), ...)
  xy <- list(
    x = .cstr_apply(unclass(x$trans$x), "scales::trans_new", ...),
    y = .cstr_apply(unclass(x$trans$y), "scales::trans_new", ...)
  )
  .cstr_apply(c(args_chr, xy), "ggplot2::coord_trans", recurse = FALSE, ...)
}

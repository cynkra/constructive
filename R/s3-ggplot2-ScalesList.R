#' @export
#' @rdname other-opts
opts_ScalesList <- function(constructor = c("ScalesList", "next", "list"), ...) {
  .cstr_options("ScalesList", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ScalesList
.cstr_construct.ScalesList <- function(x, ...) {
  opts <- list(...)$opts$ScalesList %||% opts_ScalesList()
  if (is_corrupted_ScalesList(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ScalesList", structure(NA, class = opts$constructor))
}

is_corrupted_ScalesList <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.ScalesList environment
.cstr_construct.ScalesList.environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
#' @method .cstr_construct.ScalesList ScalesList
.cstr_construct.ScalesList.ScalesList <- function(x, ...) {
  # FIXME: appropriate constructors
  if (!length(x$scales)) return(NextMethod(x))
  scales_chr <- lapply(x$scales, function(x, ...) .cstr_construct(x, ...), ...)
  if (length(x$scales) == 1) return(scales_chr[[1]])
  Reduce(function(x, y) .cstr_pipe(x, y, pipe = "plus", indent = FALSE), scales_chr)
}

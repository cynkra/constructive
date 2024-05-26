constructors$ScalesList <- new.env()

#' @export
#' @rdname other-opts
opts_ScalesList <- function(constructor = c("ScalesList", "next", "list"), ...) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "ScalesList"),
    check_dots_empty()
  )
  .cstr_options("ScalesList", constructor = constructor)
}

#' @export
.cstr_construct.ScalesList <- function(x, opts, ...) {
  opts_local <- opts$ScalesList %||% opts_ScalesList()
  if (is_corrupted_ScalesList(x) || opts_local$constructor == "next") return(NextMethod())
  constructor <- constructors$ScalesList[[opts_local$constructor]]
  constructor(x, opts = opts, ...)
}

is_corrupted_ScalesList <- function(x) {
  # TODO
  FALSE
}

constructors$ScalesList$environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
constructors$ScalesList$ScalesList <- function(x, ...) {
  # FIXME: appropriate constructors
  if (!length(x$scales)) return(NextMethod(x))
  scales_chr <- lapply(x$scales, .cstr_construct, ...)
  if (length(x$scales) == 1) return(scales_chr[[1]])
  Reduce(function(x, y) .cstr_pipe(x, y, pipe = "plus", indent = FALSE), scales_chr)
}

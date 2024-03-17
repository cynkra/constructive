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
.cstr_construct.ScalesList <- function(x, ...) {
  opts <- .cstr_fetch_opts("ScalesList", ...)
  if (is_corrupted_ScalesList(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$ScalesList[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_ScalesList <- function(x) {
  # TODO
  FALSE
}

constructors$ScalesList$environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
constructors$ScalesList$ScalesList <- function(x, ..., one_liner) {
  # FIXME: appropriate constructors
  if (!length(x$scales)) return(NextMethod(x))
  scales_chr <- lapply(x$scales, .cstr_construct,  one_liner = one_liner, ...)
  if (length(x$scales) == 1) return(scales_chr[[1]])
  Reduce(function(x, y) .cstr_pipe(x, y, pipe = "plus", one_liner = one_liner, indent = FALSE), scales_chr)
}

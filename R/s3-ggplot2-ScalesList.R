constructors$scalesList <- new.env()

#' @export
opts_scalesList <- function(constructor = c("scalesList", "next", "list"), ...) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "scalesList"),
    ellipsis::check_dots_empty()
  )
  .cstr_options("scalesList", constructor = constructor)
}

#' @export
.cstr_construct.scalesList <- function(x, ...) {
  opts <- .cstr_fetch_opts("scalesList", ...)
  if (is_corrupted_scalesList(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$scalesList[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_scalesList <- function(x) {
  # TODO
  FALSE
}

constructors$scalesList$environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
constructors$scalesList$scalesList <- function(x, ..., one_liner) {
  # FIXME: appropriate constructors
  if (!length(x$scales)) return(NextMethod(x))
  scales_chr <- lapply(x$scales, .cstr_construct,  one_liner = one_liner, ...)
  if (length(x$scales) == 1) return(scales_chr[[1]])
  Reduce(function(x, y) .cstr_pipe(x, y, pipe = "plus", one_liner = one_liner, indent = FALSE), scales_chr)
}

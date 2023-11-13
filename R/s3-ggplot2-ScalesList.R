#' @export
.cstr_construct.ScalesList <- function(x, ..., one_liner) {
  scales_chr <- lapply(x$scales, .cstr_construct,  one_liner = one_liner, ...)
  if (length(x$scales) == 1) return(scales_chr[[1]])
  Reduce(function(x, y) .cstr_pipe(x, y, pipe = "plus", one_liner = one_liner, indent = FALSE), scales_chr)
}

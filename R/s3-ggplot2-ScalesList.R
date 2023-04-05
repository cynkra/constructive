#' @export
construct_idiomatic.ScalesList <- function(x, ..., one_liner) {
  scales_chr <- lapply(x$scales, construct_raw,  one_liner = one_liner, ...)
  if(length(x$scales) == 1) return(scales_chr[[1]])
  Reduce(function(x,y) pipe(x, y, pipe = "plus", one_liner = one_liner), scales_chr)
}
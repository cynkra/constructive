#' @export
construct_idiomatic.ScalesList <- function(x, ScalesList.as_sum_of_scales = FALSE, ...) {
  if (!ScalesList.as_sum_of_scales) abort("not supported yet")
  scales_chr <- lapply(x$scales, construct_raw, ...)
  if(length(x$scales) == 1) return(scales_chr[[1]])
  Reduce(function(x,y) pipe(x, y, pipe = "plus"), scales_chr)
}

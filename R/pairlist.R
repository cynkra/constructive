#' @export
construct_idiomatic.pairlist <- function(x, ..., max_list) {
  if (!is.null(max_list)) return(trim_list(x, ..., max_list = max_list))
  construct_apply(x, "pairlist", ..., max_list = NULL)
}

#' @export
construct_idiomatic.list <- function(x, max_list, ...) {
  if (!is.null(max_list)) return(trim_list(x, max_list = max_list, ...))
  construct_apply(x, max_list = NULL, ...)
}

trim_list <- function(x, max_list, ...) {
  if (max_list == 0) return("list()")
  l <- length(x)
  if (max_list >= l) return(construct_apply(x, max_list = NULL, ...))
  code <- construct_apply(
    c(lapply(x[seq_len(max_list)], construct_raw, ...), list(paste0("+", l - max_list))),
    "list",
    new_line = FALSE,
    language = TRUE,
    max_list = max_list,
    ...
  )
  code
}

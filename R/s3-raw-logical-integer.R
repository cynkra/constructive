construct_raw_logical_integer <- function(x, ...) {
  code <- sapply(x, deparse)
  if (!all(is.na(x))) {
    code[is.na(x)] <- "NA"
  }
  if (length(x) == 1 && is.null(names(x))) {
    return(code)
  }
  code <- .cstr_apply(
    code,
    "c",
    ...,
    recurse = FALSE
  )
  if (list(...)$one_liner) code <- paste(code, collapse = " ")
  return(code)
}

#' @export
construct_idiomatic.tbl_df <- function(x, keep_trailing_comma = TRUE, tribble = FALSE, ...) {
  construct_tribble(x, tribble, ...) %||%
    construct_apply(x, fun = "tibble::tibble", keep_trailing_comma = keep_trailing_comma, ...)
}

construct_tribble <- function(x, tribble, ...) {
  if (!tribble) return(NULL)
  is_unsupported_col <- function(col) {
    is.data.frame(col) || (is.list(col) && all(lengths(col) == 1))
  }
  some_cols_are_unsupported <- any(sapply(x, is_unsupported_col))
  if (some_cols_are_unsupported) {
    warn("A tibble couldn't be built using `tibble::tribble()`, using `tibble::tibble()` instead")
    return(NULL)
  }
  code_df <- x
  code_df[] <- lapply(x, function(col) paste0(sapply(col, function(cell) paste(construct_raw(cell, ...), collapse = "")), ","))
  code_df <- rbind(paste0("~", sapply(names(x), protect), ","), as.data.frame(code_df))
  code_df[] <- lapply(code_df, format)
  code <- c("tibble::tribble(", do.call(paste, code_df), ")")
  code
}

#' @export
repair_attributes.tbl_df <- function(x, code, pipe = "base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    ignore = c("row.names"),
    idiomatic_class = c("tbl_df", "tbl", "data.frame")
  )
}

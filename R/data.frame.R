#' @export
construct_idiomatic.data.frame <- function(x, ...) {
  df_has_list_cols <- any(sapply(x, is.list))
  # FIXME: not safe re attributes
  if(df_has_list_cols) {
    tibble_code <- construct_apply(x, fun = "tibble::tibble", keep_trailing_comma = FALSE, ...)
    df_code <- wrap(tibble_code, "as.data.frame", new_line = FALSE)
    return(df_code)
  }
  construct_apply(x, fun = "data.frame", ...)
}

#' @export
repair_attributes.data.frame <- function(x, code, pipe = "base") {
  repair_attributes_impl(
    x, code, pipe,
    ignore = "row.names",
    idiomatic_class = c("data.frame")
  )
}





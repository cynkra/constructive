#' @export
construct_idiomatic.data.frame <- function(x, read.table = FALSE, ...) {
  df_has_list_cols <- any(sapply(x, is.list))
  # FIXME: not safe re attributes
  if(df_has_list_cols) {
    tibble_code <- construct_apply(x, fun = "tibble::tibble", keep_trailing_comma = FALSE, read.table = read.table, ...)
    df_code <- wrap(tibble_code, "as.data.frame", new_line = FALSE)
    return(df_code)
  }
  if (read.table && !any(lengths(lapply(x, attributes)))) {
    code_df <- x
    code_df[] <- lapply(x, as.character)
    dbl_cols <- sapply(x, is.double)
    code_df[dbl_cols] <- lapply(code_df[dbl_cols], function(col) sub("^(\\d+)$", "\\1.", col))
    code_df <- rbind(names(x), code_df)
    rn <- rownames(x)
    if (is.character(attr(x, "row.names"))) code_df <- cbind(c("", sprintf("'%s'", rownames(x))), code_df)
    code_df[] <- lapply(code_df, format)
    code <- c("read.table(header = TRUE, text = \"", do.call(paste, code_df), "\")")
    code
    return(code)
  }

  construct_apply(x, fun = "data.frame", read.table = read.table, ...)
}

#' @export
repair_attributes.data.frame <- function(x, code, pipe = "base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    ignore = "row.names",
    idiomatic_class = c("data.frame"),
    ...
  )
}





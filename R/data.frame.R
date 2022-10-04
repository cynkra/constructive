#' Constructive options for class 'data.frame'
#'
#' @param read.table Boolean, whether to build data frames using `read.table()` whenever possible
#'
#' @return An object of class <constructive_options/constructive_options_data.frame>
#' @export
opts_data.frame <- function(read.table = FALSE) {
  abort_not_boolean(read.table)
  structure(
    class = c("constructive_options", "constructive_options_data.frame"),
    list(
      read.table = read.table
    )
  )
}

#' @export
construct_idiomatic.data.frame <- function(x, ...) {
  args <- fetch_opts("data.frame", ...)
  df_has_list_cols <- any(sapply(x, function(col) is.list(col) && ! inherits(col, "AsIs")))
  # FIXME: not safe re attributes
  if (df_has_list_cols) {
    tibble_code <- construct_apply(x, fun = "tibble::tibble", ...)
    df_code <- wrap(tibble_code, "as.data.frame", new_line = FALSE)
    return(df_code)
  }
  if (args$read.table && !any(lengths(lapply(x, attributes)))) {
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

  some_names_are_non_syntactic <- any(!is_syntactic(names(x)))
  if (some_names_are_non_syntactic) x <- c(x, list(check.names = FALSE))
  construct_apply(x, fun = "data.frame", ...)
}

#' @export
repair_attributes.data.frame <- function(x, code, ..., pipe = "base") {
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = if (identical(attr(x, "row.names"), seq_len(nrow(x)))) "row.names",
    idiomatic_class = c("data.frame")
  )
}





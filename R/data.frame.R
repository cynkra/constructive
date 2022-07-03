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

#' @export
construct_idiomatic.data.table <- function(x, ...) {
  construct_apply(x, fun = "data.table", keep_trailing_comma = FALSE, ...)
}

#' @export
repair_attributes.data.table <- function(x, code, pipe = "base") {
  repair_attributes_impl(
    x, code, pipe,
    ignore = c("row.names", ".internal.selfref"),
    idiomatic_class = c("data.table", "data.frame")
  )
}

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
  code_df <- rbind(paste0("~", sapply(names(x), protect), ","), code_df)
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


# no need for a constructor for grouped_df since it falls back on tbl_df
#' @export
repair_attributes.grouped_df <- function(x, code, pipe = "base", ...) {
  grps <- head(names(attr(x, "groups")), -1)
  group_by_code <- construct_apply(
    grps,
    "dplyr::group_by",
    language = TRUE,
    ...
  )
  code <- pipe(
    code,
    group_by_code,
    pipe = pipe
  )
  repair_attributes_impl(
    x, code, pipe,
    ignore = c("row.names", "groups"),
    idiomatic_class = c("grouped_df", "tbl_df", "tbl", "data.frame")
  )
}

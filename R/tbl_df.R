#' Constructive options for class 'tbl_df'
#'
#' @param tribble Boolean, whether to build tibbles using `tribble()` whenever possible
#' @param trailing_comma Boolean, whether to leave a trailing comma at the end of tibble
#' calls
#'
#' @return An object of class <constructive_options/constructive_options_tbl_df>
#' @export
opts_tbl_df <- function(tribble = FALSE, trailing_comma = TRUE) {
  combine_errors(
    abort_not_boolean(tribble),
    abort_not_boolean(trailing_comma)
  )
  structure(
    class = c("constructive_options", "constructive_options_tbl_df"),
    list(
      tribble = tribble,
      trailing_comma = trailing_comma
    )
  )
}

#' @export
construct_idiomatic.tbl_df <- function(x, ...) {
  opts <- fetch_opts("tbl_df", ...)
  construct_tribble(x, ..., tribble = opts$tribble, trailing_comma = opts$trailing_comma) %||%
    construct_apply(x, fun = "tibble::tibble", ..., keep_trailing_comma = opts$trailing_comma)
}

construct_tribble <- function(x, ..., tribble, trailing_comma) {
  if (!tribble) return(NULL)
  is_unsupported_col <- function(col) {
    is.data.frame(col) || (is.list(col) && all(lengths(col) == 1))
  }
  some_cols_are_unsupported <- any(sapply(x, is_unsupported_col))
  if (some_cols_are_unsupported) {
    return(NULL)
  }
  code_df <- x
  code_df[] <- lapply(x, function(col) paste0(sapply(col, function(cell) paste(construct_raw(cell, ...), collapse = "")), ","))
  code_df <- rbind(paste0("~", sapply(names(x), protect), ","), as.data.frame(code_df))
  code_df[] <- lapply(code_df, format)
  code <- do.call(paste, code_df)
  if (!trailing_comma) {
    code[[length(code)]] <- sub(", *$", "", code[[length(code)]])
  }
  code <- c("tibble::tribble(", code, ")")
  code
}

#' @export
repair_attributes.tbl_df <- function(x, code, ..., pipe = "base") {
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = c("row.names"),
    idiomatic_class = c("tbl_df", "tbl", "data.frame")
  )
}

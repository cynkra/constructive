#' Constructive options for tibbles
#'
#' These options will be used on objects of class 'tbl_df', also known as tibbles. .
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"tibble"` (default): Wrap the column definitions in a `tibble::tibble()` call.
#' * `"tribble"` : We build the object using `tibble::tribble()` if possible, and fall
#'   back to `tibble::tibble()`.
#' * `"list"` : Use `list()` and treat the class as a regular attribute.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @param trailing_comma Boolean, whether to leave a trailing comma at the end of the constructor call
#' calls
#'
#' @return An object of class <constructive_options/constructive_options_tbl_df>
#' @export
opts_tbl_df <- function(constructor = c("tibble", "tribble", "list"), ..., trailing_comma = TRUE) {
  combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty(),
    abort_not_boolean(trailing_comma)
  )
  constructive_options("tbl_df", constructor = constructor, trailing_comma = trailing_comma)
}

#' @export
construct_idiomatic.tbl_df <- function(x, ...) {
  opts <- fetch_opts("tbl_df", ...)
  if (opts$constructor == "list") {
    return(construct_idiomatic.list(x, ...))
  }
  constructor <- opts$constructor
  trailing_comma <- opts$trailing_comma
  if (constructor == "tribble" && nrow(x)) {
    is_unsupported_col <- function(col) {
      is.data.frame(col) || (is.list(col) && all(lengths(col) == 1))
    }
    some_cols_are_unsupported <- any(sapply(x, is_unsupported_col))
    if (!some_cols_are_unsupported) {
      code <- construct_tribble(x, ..., trailing_comma = trailing_comma)
      return(code)
    }
  }
  construct_apply(x, fun = "tibble::tibble", ..., keep_trailing_comma = opts$trailing_comma)
}

construct_tribble <- function(x, ..., trailing_comma) {
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
  opts <- fetch_opts("tbl_df", ...)
  if (opts$constructor == "list") {
    return(repair_attributes.default(x, code, ..., pipe = pipe))
  }
  ignore <- "row.names"
  if (identical(names(x), character())) ignore <- c(ignore, "names")
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = ignore,
    idiomatic_class = c("tbl_df", "tbl", "data.frame")
  )
}

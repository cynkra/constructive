constructors$tbl_df <- new.env()

#' Constructive options for tibbles
#'
#' These options will be used on objects of class 'tbl_df', also known as tibbles. .
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"tibble"` (default): Wrap the column definitions in a `tibble::tibble()` call.
#' * `"tribble"` : We build the object using `tibble::tribble()` if possible, and fall
#'   back to `tibble::tibble()`.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"list"` : Use `list()` and treat the class as a regular attribute.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @param trailing_comma Boolean, whether to leave a trailing comma at the end of the constructor call
#' calls
#'
#' @return An object of class <constructive_options/constructive_options_tbl_df>
#' @export
opts_tbl_df <- function(constructor = c("tibble", "tribble", "next", "list"), ..., trailing_comma = TRUE) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "tbl_df"),
    ellipsis::check_dots_empty(),
    abort_not_boolean(trailing_comma)
  )
  .cstr_options("tbl_df", constructor = constructor, trailing_comma = trailing_comma)
}

#' @export
.cstr_construct.tbl_df <- function(x, ...) {
  opts <- .cstr_fetch_opts("tbl_df", ...)
  if (is_corrupted_tbl_df(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$tbl_df[[opts$constructor]]
  constructor(x, ..., trailing_comma = opts$trailing_comma)
}

is_corrupted_tbl_df <- function(x) {
  # FIXME: ?rownames says a tibble can have rownames but as_tibble(mtcars) removes them
  is_corrupted_data.frame(x)
}

constructors$tbl_df$list <- function(x, ..., trailing_comma = TRUE) {
  .cstr_construct.list(x, ...)
}

constructors$tbl_df$tibble <- function(x, ..., trailing_comma = TRUE) {
  # construct idiomatic code
  code <- .cstr_apply(x, fun = "tibble::tibble", ..., trailing_comma = trailing_comma)

  # repair
  repair_attributes_tbl_df(x, code, ...)
}

constructors$tbl_df$tribble <- function(x, ..., trailing_comma = TRUE) {
  # fall back to tibble if no row or has df cols or list cols containing only length 1 elements
  if (!nrow(x)) return(constructors$tbl_df$tibble(x, ...))
  is_unsupported_col <- function(col) {
    is.data.frame(col) || (is.list(col) && all(lengths(col) == 1))
  }
  some_cols_are_unsupported <- any(sapply(x, is_unsupported_col))
  if (some_cols_are_unsupported) return(constructors$tbl_df$tibble(x, ...))

  # construct idiomatic code
  code_df <- x
  code_df[] <- lapply(x, function(col) paste0(sapply(col, function(cell) paste(.cstr_construct(cell, ...), collapse = "")), ","))
  code_df <- rbind(paste0("~", sapply(names(x), protect), ","), as.data.frame(code_df))
  code_df[] <- lapply(code_df, format)
  code <- do.call(paste, code_df)
  if (!trailing_comma) {
    code[[length(code)]] <- sub(", *$", "", code[[length(code)]])
  }
  code <- c("tibble::tribble(", code, ")")

  # repair
  repair_attributes_tbl_df(x, code, ...)
}

repair_attributes_tbl_df <- function(x, code, ..., pipe = "base") {
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = "row.names",
    idiomatic_class = c("tbl_df", "tbl", "data.frame")
  )
}

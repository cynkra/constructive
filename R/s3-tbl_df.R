#' Constructive options for tibbles
#'
#' These options will be used on objects of class 'tbl_df', also known as tibbles.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"tibble"` (default): Wrap the column definitions in a `tibble::tibble()` call.
#' * `"tribble"` : We build the object using `tibble::tribble()` if possible, and fall
#'   back to `tibble::tibble()`.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"list"` : Use `list()` and treat the class as a regular attribute.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @param trailing_comma Boolean. Whether to leave a trailing comma at the end of the constructor call
#' calls
#' @param justify String. Justification for columns if `constructor` is `"tribble"`
#'
#' @return An object of class <constructive_options/constructive_options_tbl_df>
#' @export
opts_tbl_df <- function(constructor = c("tibble", "tribble", "next", "list"),
                        ...,
                        trailing_comma = TRUE,
                        justify = c("left", "right", "centre", "none")) {
  .cstr_combine_errors(
    abort_not_boolean(trailing_comma),
    justify <- match.arg(justify)
  )
  .cstr_options("tbl_df", constructor = constructor[[1]], ..., trailing_comma = trailing_comma, justify = justify)
}

#' @export
#' @method .cstr_construct tbl_df
.cstr_construct.tbl_df <- function(x, ...) {
  opts <- list(...)$opts$tbl_df %||% opts_tbl_df()
  if (is_corrupted_tbl_df(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.tbl_df", structure(NA, class = opts$constructor))
}

is_corrupted_tbl_df <- function(x) {
  # FIXME: ?rownames says a tibble can have rownames but as_tibble(mtcars) removes them
  is_corrupted_data.frame(x) || !identical(attr(x, "row.names"), seq_len(nrow(x)))
}

#' @export
#' @method .cstr_construct.tbl_df list
.cstr_construct.tbl_df.list <- function(x, ...) {
  opts <- list(...)$opts$tbl_df %||% opts_tbl_df()
  .cstr_construct.list(x, ...)
}

#' @export
#' @method .cstr_construct.tbl_df tibble
.cstr_construct.tbl_df.tibble <- function(x, ...) {
  opts <- list(...)$opts$tbl_df %||% opts_tbl_df()
  arg_names <- c(".rows", ".name_repair ")
  df_has_problematic_names <- any(names(x) %in% arg_names)
  if (df_has_problematic_names) return(.cstr_construct.list(x, ...))
  # construct idiomatic code
  code <- .cstr_apply(x, fun = "tibble::tibble", ..., trailing_comma = opts$trailing_comma)

  # repair
  repair_attributes_tbl_df(x, code, ...)
}

#' @export
#' @method .cstr_construct.tbl_df tribble
.cstr_construct.tbl_df.tribble <- function(x, ...) {
  opts <- list(...)$opts$tbl_df %||% opts_tbl_df()
  # fall back to tibble if no row or has df cols or list cols containing only length 1 elements
  if (!nrow(x)) return(.cstr_construct.tbl_df.tibble(x, ...))
  is_unsupported_col <- function(col) {
    is.data.frame(col) || (is.list(col) && all(lengths(col) == 1))
  }
  some_cols_are_unsupported <- any(sapply(x, is_unsupported_col))
  if (some_cols_are_unsupported) return(.cstr_construct.tbl_df.tibble(x, ...))

  # construct idiomatic code
  code_df <- x
  code_df[] <- lapply(x, function(col) paste0(sapply(col, function(cell) paste(.cstr_construct(cell, ...), collapse = "")), ","))
  code_df <- rbind(paste0("~", sapply(names(x), protect), ","), as.data.frame(code_df))
  code_df[] <- lapply(code_df, format, justify = opts$justify)
  code <- do.call(paste, code_df)
  if (!opts$trailing_comma) {
    code[[length(code)]] <- sub(", *$", "", code[[length(code)]])
  }
  code <- sub(" +$", "", code)
  code <- c(
    "tibble::tribble(",
    indent(code),
    ")"
  )

  # repair
  repair_attributes_tbl_df(x, code, ...)
}

repair_attributes_tbl_df <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = "row.names",
    idiomatic_class = c("tbl_df", "tbl", "data.frame")
  )
}

#' Constructive options for class 'data.frame'
#'
#' These options will be used on objects of class 'data.frame'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"data.frame"` (default): Wrap the column definitions in a `data.frame()` call. If some
#'   columns are lists or data frames, we wrap the column definitions in `tibble::tibble()`.
#'   then use `as.data.frame()`.
#' * `"read.table"` : We build the object using `read.table()` if possible, and fall
#'   back to `data.frame()`.
#' * `"list"` : Use `list()` and treat the class as a regular attribute.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_data.frame>
#' @export
opts_data.frame <- function(constructor = c("data.frame", "read.table", "list"), ...) {
  combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  constructive_options("data.frame", constructor = constructor)
}

#' @export
construct_idiomatic.data.frame <- function(x, ...) {
  opts <- fetch_opts("data.frame", ...)
  if (opts$constructor == "list") {
    return(construct_idiomatic.list(x, ...))
  }
  df_has_list_cols <- any(sapply(x, function(col) is.list(col) && ! inherits(col, "AsIs")))
  # FIXME: not safe re attributes
  if (df_has_list_cols) {
    tibble_code <- construct_apply(x, fun = "tibble::tibble", ...)
    df_code <- wrap(tibble_code, "as.data.frame", new_line = FALSE)
    return(df_code)
  }
  if (opts$constructor == "read.table" && !any(lengths(lapply(x, attributes)))) {
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

  rn <- attr(x, "row.names")
  if (!identical(rn, seq_len(nrow(x)))) x <- c(x, list(row.names = rn))
  if (any(!is_syntactic(names(x)))) x <- c(x, list(check.names = FALSE))
  construct_apply(x, fun = "data.frame", ...)
}

#' @export
repair_attributes.data.frame <- function(x, code, ..., pipe = "base") {
  opts <- fetch_opts("data.frame", ...)
  if (opts$constructor == "list") {
    return(repair_attributes.default(x, code, ..., pipe = pipe))
  }
  ignore <- "row.names"
  if (identical(names(x), character())) ignore <- c(ignore, "names")
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = ignore,
    idiomatic_class = c("data.frame")
  )
}





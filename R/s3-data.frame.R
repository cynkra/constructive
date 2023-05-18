constructors$data.frame <- new.env()

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
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"list"` : Use `list()` and treat the class as a regular attribute.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_data.frame>
#' @export
opts_data.frame <- function(constructor = c("data.frame", "read.table", "next", "list"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  .cstr_options("data.frame", constructor = constructor)
}

#' @export
.cstr_construct.data.frame <- function(x, ...) {
  opts <- .cstr_fetch_opts("data.frame", ...)
  if (is_corrupted_data.frame(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$data.frame[[opts$constructor]]
  constructor(x, ...)
}

#' @export
is_corrupted_data.frame <- function(x) {
  if (!is.list(x) || any(sapply(unclass(x), is.null))) return(TRUE)
  attrs <- attributes(x)
  if (!all(c("names", "class", "row.names") %in% names(attrs))) return(TRUE)
  if (!is.character(attrs$names) || length(attrs$names) != length(x)) return(TRUE)
  elements_and_row_names_all_have_same_length <-
    length(unique(vapply(c(list(attrs$row.names), x), NROW, integer(1)))) == 1
  if (!elements_and_row_names_all_have_same_length) return(TRUE)
  FALSE
}

constructors$data.frame$list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

constructors$data.frame$read.table <- function(x, ...) {
  # Fall back on data.frame constructor if relevant
  rn <- attr(x, "row.names")
  numeric_row_names_are_not_default <- is.numeric(rn) && !identical(rn, seq_len(nrow(x)))
  if (numeric_row_names_are_not_default) return(constructors$data.frame$data.frame(x, ...))

  some_cols_are_not_atomic_vectors <-
    any(!vapply(x, function(x) is.atomic(x) && is.vector(x), logical(1)))
  if (some_cols_are_not_atomic_vectors) return(constructors$data.frame$data.frame(x, ...))

  some_cols_are_char_containing_nums <-
    any(vapply(x, function(x) is.character(x) && !any(is.na(suppressWarnings(as.numeric(x)))), logical(1)))
  if (some_cols_are_char_containing_nums) return(constructors$data.frame$data.frame(x, ...))

  # fill a data frame with deparsed values
  code_df <- x
  code_df[] <- lapply(x, as.character)
  dbl_cols <- sapply(x, is.double)

  # make sure double values will be read as double by adding a dot at the end of integerish values
  code_df[dbl_cols] <- lapply(code_df[dbl_cols], function(col) sub("^(\\d+)$", "\\1.", col))

  # include headers and row names in the table
  code_df <- rbind(names(x), code_df)
  rn <- rownames(x)
  if (is.character(attr(x, "row.names"))) {
    code_df <- cbind(c("", sprintf("'%s'", rownames(x))), code_df)
  }
  code_df[] <- lapply(code_df, format)

  # collapse table into code
  code <- c("read.table(header = TRUE, text = \"", do.call(paste, code_df), "\")")

  # repair
  repair_attributes.data.frame(x, code, ...)
}

constructors$data.frame$data.frame <- function(x, ...) {
  # Fall back on list constructor if relevant
  df_has_list_cols <- any(sapply(x, function(col) is.list(col) && ! inherits(col, "AsIs")))
  if (df_has_list_cols) return(.cstr_construct.list(x, ...))

  args <- x

  # include row.names arg only if necessary
  rn <- attr(x, "row.names")
  if (!identical(rn, seq_len(nrow(x)))) args <- c(args, list(row.names = rn))

  # include check.names arg only if necessary
  if (any(!is_syntactic(names(x)))) args <- c(args, list(check.names = FALSE))

  # build code recursively
  code <- .cstr_apply(args, fun = "data.frame", ...)

  # repair
  repair_attributes.data.frame(x, code, ...)
}

#' @export
repair_attributes.data.frame <- function(x, code, ..., pipe = "base") {
  ignore <- "row.names"
  if (identical(names(x), character())) ignore <- c(ignore, "names")
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = ignore,
    idiomatic_class = c("data.frame")
  )
}

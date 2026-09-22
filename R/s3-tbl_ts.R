#' Constructive options for class 'tbl_ts'
#'
#' These options will be used on objects of class 'tbl_ts', also known as tsibbles.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"tsibble"` (default): Wrap the column definitions in a `tsibble::tsibble()`
#'   call. If some column names conflict with the arguments of `tsibble::tsibble()`
#'   we fall back to `"as_tsibble"`.
#' * `"as_tsibble"` : Construct the underlying tibble and pipe it into
#'   `tsibble::as_tsibble()`. Use `opts_tbl_df()` to tweak the construction
#'   of the tibble.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' The "interval" attribute is always recomputed by 'tsibble', we only set
#' `regular = FALSE` for irregular tsibbles.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_tbl_ts>
#' @export
opts_tbl_ts <- function(constructor = c("tsibble", "as_tsibble", "next"), ...) {
  .cstr_options("tbl_ts", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct tbl_ts
.cstr_construct.tbl_ts <- function(x, ...) {
  opts <- list(...)$opts$tbl_ts %||% opts_tbl_ts()
  if (is_corrupted_tbl_ts(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.tbl_ts", structure(NA, class = opts$constructor))
}

is_corrupted_tbl_ts <- function(x) {
  if (is_corrupted_tbl_df(x)) return(TRUE)
  key <- attr(x, "key")
  if (!is.data.frame(key) || !identical(tail(names(key), 1), ".rows")) return(TRUE)
  if (!all(head(names(key), -1) %in% names(x))) return(TRUE)
  index <- attr(x, "index")
  if (!is.character(index) || length(index) != 1 || !index %in% names(x)) return(TRUE)
  # index2 differs from index after `tsibble::index_by()`, which gives a
  # "grouped_ts" object, handled by `.cstr_construct.grouped_ts()`
  if (!identical(attr(x, "index2"), as.character(index))) return(TRUE)
  if (!inherits(attr(x, "interval"), "interval")) return(TRUE)
  # tsibble sorts rows by key and index with `dplyr::arrange()`, which uses `vctrs::vec_order()`
  sort_vars <- vctrs::new_data_frame(.subset(x, c(head(names(key), -1), index)))
  !identical(vctrs::vec_order(sort_vars), seq_len(nrow(x)))
}

#' @export
#' @method .cstr_construct.tbl_ts tsibble
.cstr_construct.tbl_ts.tsibble <- function(x, ...) {
  arg_names <- c("key", "index", "regular", ".drop")
  if (any(names(x) %in% c(arg_names, "", NA)) || anyDuplicated(names(x))) {
    return(.cstr_construct.tbl_ts.as_tsibble(x, ...))
  }
  args <- lapply(x, function(col, ...) .cstr_construct(col, ...), ...)
  args <- c(args, tsibble_args(x, ...))
  code <- .cstr_apply(args, fun = "tsibble::tsibble", ..., recurse = FALSE)
  repair_attributes_tbl_ts(x, code, ...)
}

#' @export
#' @method .cstr_construct.tbl_ts as_tsibble
.cstr_construct.tbl_ts.as_tsibble <- function(x, ...) {
  x_stripped <- x
  class(x_stripped) <- setdiff(class(x), "tbl_ts")
  attr(x_stripped, "key") <- NULL
  attr(x_stripped, "index") <- NULL
  attr(x_stripped, "index2") <- NULL
  attr(x_stripped, "interval") <- NULL
  code <- .cstr_construct(x_stripped, ...)
  as_tsibble_code <- .cstr_apply(tsibble_args(x, ...), "tsibble::as_tsibble", ..., recurse = FALSE)
  code <- .cstr_pipe(code, as_tsibble_code, ...)
  repair_attributes_tbl_ts(x, code, ...)
}

# code of the `key`, `index`, `regular` and `.drop` args of `tsibble::tsibble()`
# and `tsibble::as_tsibble()`, only when they differ from the defaults
tsibble_args <- function(x, ...) {
  key <- attr(x, "key")
  key_vars <- protect(head(names(key), -1))
  args <- list()
  if (length(key_vars) == 1) {
    args$key <- key_vars
  } else if (length(key_vars) > 1) {
    args$key <- .cstr_apply(key_vars, "c", ..., recurse = FALSE, new_line = FALSE)
  }
  args$index <- protect(as.character(attr(x, "index")))
  if (isFALSE(attr(attr(x, "interval"), ".regular"))) args$regular <- "FALSE"
  if (isFALSE(attr(key, ".drop"))) args$.drop <- "FALSE"
  args
}

repair_attributes_tbl_ts <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = c("row.names", "key", "index", "index2", "interval"),
    idiomatic_class = c("tbl_ts", "tbl_df", "tbl", "data.frame")
  )
}

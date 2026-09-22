#' Constructive options for class 'col_spec'
#'
#' These options will be used on objects of class 'col_spec'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"cols"` (default): We build the object using `readr::cols()`, or
#'   `readr::cols_only()` when the default collector is `readr::col_skip()`.
#'   The `.default` argument is omitted when it's `readr::col_guess()`.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `"list"`
#' * `"list"` : We define as a list and repair attributes
#'
#' Use `opts_collector()` to tweak the construction of the column specifications.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_col_spec>
#' @export
opts_col_spec <- function(constructor = c("cols", "next", "list"), ...) {
  .cstr_options("col_spec", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct col_spec
.cstr_construct.col_spec <- function(x, ...) {
  opts <- list(...)$opts$col_spec %||% opts_col_spec()
  if (is_corrupted_col_spec(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.col_spec", structure(NA, class = opts$constructor))
}

is_corrupted_col_spec <- function(x) {
  if (!is.list(x) || !identical(names(x), c("cols", "default", "delim"))) return(TRUE)
  if (!is.list(x$cols) || !all(names(attributes(x$cols)) %in% "names")) return(TRUE)
  # these names would be interpreted as arguments of `readr::cols()`
  if (any(c(".default", ".delim") %in% names(x$cols))) return(TRUE)
  if (!inherits(x$default, "collector")) return(TRUE)
  if (!all(vapply(x$cols, inherits, logical(1), "collector"))) return(TRUE)
  !is.null(x$delim) && !rlang::is_string(x$delim)
}

#' @export
#' @method .cstr_construct.col_spec cols
.cstr_construct.col_spec.cols <- function(x, ...) {
  args <- x$cols
  if (identical(x$default, readr::col_skip()) && is.null(x$delim)) {
    code <- .cstr_apply(args, "readr::cols_only", ...)
    return(repair_attributes_col_spec(x, code, ...))
  }
  if (!identical(x$default, readr::col_guess())) args$.default <- x$default
  args$.delim <- x$delim
  code <- .cstr_apply(args, "readr::cols", ...)
  repair_attributes_col_spec(x, code, ...)
}

#' @export
#' @method .cstr_construct.col_spec list
.cstr_construct.col_spec.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_col_spec <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "col_spec"
  )
}

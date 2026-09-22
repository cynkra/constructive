#' Constructive options for class 'grouped_ts'
#'
#' These options will be used on objects of class 'grouped_ts'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"group_by"` (default): Construct the ungrouped tsibble and pipe it into
#'   `dplyr::group_by()`, and into `tsibble::index_by()` if the object is grouped
#'   by an index. Use `opts_tbl_ts()` to tweak the construction of the tsibble.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_grouped_ts>
#' @export
opts_grouped_ts <- function(constructor = c("group_by", "next"), ...) {
  .cstr_options("grouped_ts", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct grouped_ts
.cstr_construct.grouped_ts <- function(x, ...) {
  opts <- list(...)$opts$grouped_ts %||% opts_grouped_ts()
  if (is_corrupted_grouped_ts(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.grouped_ts", structure(NA, class = opts$constructor))
}

is_corrupted_grouped_ts <- function(x) {
  if (is_corrupted_grouped_df(x)) return(TRUE)
  index2 <- attr(x, "index2")
  if (!is.character(index2) || length(index2) != 1) return(TRUE)
  group_vars <- head(names(attr(x, "groups")), -1)
  if (!all(group_vars %in% names(x))) return(TRUE)
  # after `tsibble::index_by()` the new index is the last grouping variable
  if (index2 != attr(x, "index") && !identical(tail(group_vars, 1), index2)) return(TRUE)
  is_corrupted_tbl_ts(ungroup_ts(x))
}

#' @export
#' @method .cstr_construct.grouped_ts group_by
.cstr_construct.grouped_ts.group_by <- function(x, ...) {
  code <- .cstr_construct(ungroup_ts(x), ...)
  groups <- attr(x, "groups")
  group_vars <- protect(head(names(groups), -1))
  index2 <- attr(x, "index2")
  is_index_by <- index2 != attr(x, "index")
  if (is_index_by) group_vars <- head(group_vars, -1)
  if (length(group_vars)) {
    args <- as.list(group_vars)
    if (isFALSE(attr(groups, ".drop"))) args$.drop <- "FALSE"
    group_by_code <- .cstr_apply(args, "dplyr::group_by", ..., recurse = FALSE)
    code <- .cstr_pipe(code, group_by_code, ...)
  }
  if (is_index_by) {
    index_by_code <- .cstr_apply(list(protect(index2)), "tsibble::index_by", ..., recurse = FALSE)
    code <- .cstr_pipe(code, index_by_code, ...)
  }
  repair_attributes_grouped_ts(x, code, ...)
}

# remove the grouping, including the one from `tsibble::index_by()`
ungroup_ts <- function(x) {
  class(x) <- setdiff(class(x), c("grouped_ts", "grouped_df"))
  attr(x, "groups") <- NULL
  attr(x, "index2") <- as.character(attr(x, "index"))
  x
}

repair_attributes_grouped_ts <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = c("row.names", "groups", "key", "index", "index2", "interval"),
    idiomatic_class = c("grouped_ts", "grouped_df", "tbl_ts", "tbl_df", "tbl", "data.frame")
  )
}

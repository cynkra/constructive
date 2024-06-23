#' Constructive options for class 'rowwise_df'
#'
#' These options will be used on objects of class 'rowwise_df'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"list"` : We define as an list object and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_rowwise_df>
#' @export
opts_rowwise_df <- function(constructor = c("default", "next", "list"), ...) {
  .cstr_options("rowwise_df", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct rowwise_df
.cstr_construct.rowwise_df <- function(x, ...) {
  opts <- list(...)$opts$rowwise_df %||% opts_rowwise_df()
  if (is_corrupted_rowwise_df(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.rowwise_df", structure(NA, class = opts$constructor))
}

is_corrupted_rowwise_df <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.rowwise_df default
.cstr_construct.rowwise_df.default <- function(x, ...) {
  x_stripped <- x
  class(x_stripped) <- setdiff(class(x_stripped), "rowwise_df")
  attr(x_stripped, "groups") <- NULL
  code <- .cstr_construct(x_stripped, ...)
  vars <- head(names(attr(x, "groups")), -1)
  rowwise_code <- .cstr_apply(vars, "dplyr::rowwise", ..., recurse = FALSE)
  code <- .cstr_pipe(code, rowwise_code, ...)
  repair_attributes_rowwise_df(x, code, ...)
}

#' @export
#' @method .cstr_construct.rowwise_df list
.cstr_construct.rowwise_df.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

# no need for a constructor for grouped_df since it falls back on tbl_df
repair_attributes_rowwise_df <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = c("row.names", "groups"),
    idiomatic_class = c("rowwise_df", "tbl_df", "tbl", "data.frame")
  )
}

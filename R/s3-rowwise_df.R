constructors$rowwise_df <- new.env()

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
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "rowwise_df"),
    check_dots_empty()
  )
  .cstr_options("rowwise_df", constructor = constructor)
}

#' @export
.cstr_construct.rowwise_df <- function(x, opts = NULL, ...) {
  opts_local <- opts$rowwise_df %||% opts_rowwise_df()
  if (is_corrupted_rowwise_df(x) || opts_local[["constructor"]] == "next") return(NextMethod())
  constructor <- constructors$rowwise_df[[opts_local[["constructor"]]]]
  constructor(x, opts = opts, ...)
}

is_corrupted_rowwise_df <- function(x) {
  # TODO
  FALSE
}

constructors$rowwise_df$default <- function(x, ...) {
  x_stripped <- x
  class(x_stripped) <- setdiff(class(x_stripped), "rowwise_df")
  attr(x_stripped, "groups") <- NULL
  code <- .cstr_construct(x_stripped, ...)
  vars <- head(names(attr(x, "groups")), -1)
  rowwise_code <- .cstr_apply(vars, "dplyr::rowwise", ..., recurse = FALSE)
  code <- .cstr_pipe(code, rowwise_code, ...)
  repair_attributes_rowwise_df(x, code, ...)
}

constructors$rowwise_df$list <- function(x, ...) {
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

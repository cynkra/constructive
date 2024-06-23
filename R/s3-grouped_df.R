#' Constructive options for class 'grouped_df'
#'
#' These options will be used on objects of class 'grouped_df'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"list"` : We define as an list object and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_factor>
#' @export
opts_grouped_df <- function(constructor = c("default", "next", "list"), ...) {
  .cstr_options("grouped_df", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct grouped_df
.cstr_construct.grouped_df <- function(x, ...) {
  opts <- list(...)$opts$grouped_df %||% opts_grouped_df()
  if (is_corrupted_grouped_df(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.grouped_df", structure(NA, class = opts$constructor))
}

is_corrupted_grouped_df <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.grouped_df default
.cstr_construct.grouped_df.default <- function(x, ...) {
  x_stripped <- x
  class(x_stripped) <- setdiff(class(x_stripped), "grouped_df")
  attr(x_stripped, "groups") <- NULL
  code <- .cstr_construct(x_stripped, ...)
  grps <- head(names(attr(x, "groups")), -1)
  group_by_code <- .cstr_apply(grps, "dplyr::group_by", ..., recurse = FALSE)
  code <- .cstr_pipe(code, group_by_code, ...)
  repair_attributes_grouped_df(x, code, ...)
}

#' @export
#' @method .cstr_construct.grouped_df list
.cstr_construct.grouped_df.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_grouped_df <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = c("row.names", "groups"),
    idiomatic_class = c("grouped_df", "tbl_df", "tbl", "data.frame")
  )
}

constructors$grouped_df <- new.env()

#' Constructive options for class 'grouped_df'
#'
#' These options will be used on objects of class 'grouped_df'.
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"list"` : We define as an list object and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_factor>
#' @export
opts_grouped_df <- function(constructor = c("default", "next", "list"), ...) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "grouped_df"),
    check_dots_empty()
  )
  .cstr_options("grouped_df", constructor = constructor)
}

#' @export
.cstr_construct.grouped_df <- function(x, ...) {
  opts <- .cstr_fetch_opts("grouped_df", ...)
  if (is_corrupted_grouped_df(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$grouped_df[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_grouped_df <- function(x) {
  # TODO
  FALSE
}

constructors$grouped_df$default <- function(x, ..., one_liner, pipe) {
  x_stripped <- x
  class(x_stripped) <- setdiff(class(x_stripped), "grouped_df")
  attr(x_stripped, "groups") <- NULL
  code <- .cstr_construct(x_stripped, ...)
  grps <- head(names(attr(x, "groups")), -1)
  group_by_code <- .cstr_apply(
    grps,
    "dplyr::group_by",
    ...,
    recurse = FALSE,
    pipe = pipe,
    one_liner = one_liner
  )
  code <- .cstr_pipe(
    code,
    group_by_code,
    pipe = pipe,
    one_liner = one_liner
  )
  repair_attributes_grouped_df(x, code, ..., one_liner = one_liner, pipe = pipe)
}

constructors$grouped_df$list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_grouped_df <- function(x, code, ..., pipe = NULL, one_liner = FALSE) {
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = c("row.names", "groups"),
    idiomatic_class = c("grouped_df", "tbl_df", "tbl", "data.frame"),
    one_liner = one_liner
  )
}

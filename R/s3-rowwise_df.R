constructors$rowwise_df <- new.env()

#' Constructive options for class 'rowwise_df'
#'
#' These options will be used on objects of class 'rowwise_df'.
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"list"` : We define as an list object and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @param origin Origin to be used, ignored when irrelevant.
#'
#' @return An object of class <constructive_options/constructive_options_factor>
#' @export
opts_rowwise_df <- function(constructor = c("default", "next", "list"), ..., origin = "1970-01-01") {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  .cstr_options("rowwise_df", constructor = constructor, origin = origin)
}

#' @export
.cstr_construct.rowwise_df <- function(x, ...) {
  opts <- .cstr_fetch_opts("rowwise_df", ...)
  if (is_corrupted_rowwise_df(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$rowwise_df[[opts$constructor]]
  constructor(x, ...)
}

#' @export
is_corrupted_rowwise_df <- function(x) {
  # TODO
  FALSE
}

constructors$rowwise_df$default <- function(x, ..., one_liner, pipe) {
  x_stripped <- x
  class(x_stripped) <- setdiff(class(x_stripped), "rowwise_df")
  attr(x_stripped, "groups") <- NULL
  code <- .cstr_construct(x_stripped, ...)
  vars <- head(names(attr(x, "groups")), -1)
  rowwise_code <- .cstr_apply(
    vars,
    "dplyr::rowwise",
    ...,
    recurse = FALSE
  )
  code <- .cstr_pipe(
    code,
    rowwise_code,
    one_liner = one_liner,
    pipe = pipe
  )
  repair_attributes.rowwise_df(x, code, ...)
}

constructors$rowwise_df$list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

# no need for a constructor for grouped_df since it falls back on tbl_df
#' @export
repair_attributes.rowwise_df <- function(x, code, ..., pipe = "base", one_liner = FALSE) {
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = c("row.names", "groups"),
    idiomatic_class = c("rowwise_df", "tbl_df", "tbl", "data.frame"),
    one_liner = one_liner
  )
}


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
#' @param origin Origin to be used, ignored when irrelevant.
#'
#' @return An object of class <constructive_options/constructive_options_factor>
#' @export
opts_grouped_df <- function(constructor = c("default", "next", "list"), ..., origin = "1970-01-01") {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  constructive_options("grouped_df", constructor = constructor, origin = origin)
}

#' @export
construct_raw.grouped_df <- function(x, ...) {
  opts <- fetch_opts("grouped_df", ...)
  if (is_corrupted_grouped_df(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$grouped_df[[opts$constructor]]
  constructor(x, ...)
}

#' @export
is_corrupted_grouped_df <- function(x) {
  # TODO
  FALSE
}

constructors$grouped_df$default <- function(x, ..., one_liner, pipe) {
  x_stripped <- x
  class(x_stripped) <- setdiff(class(x_stripped), "grouped_df")
  attr(x_stripped, "groups") <- NULL
  code <- construct_raw(x_stripped, ...)
  grps <- head(names(attr(x, "groups")), -1)
  group_by_code <- .cstr_apply(
    grps,
    "dplyr::group_by",
    ...,
    language = TRUE,
    pipe = pipe,
    one_liner = one_liner
  )
  code <- pipe(
    code,
    group_by_code,
    pipe = pipe,
    one_liner = one_liner
  )
  repair_attributes.grouped_df(x, code, ..., one_liner = one_liner, pipe = pipe)
}

constructors$grouped_df$list <- function(x, ...) {
  construct_raw.list(x, ...)
}

# no need for a constructor for grouped_df since it falls back on tbl_df
#' @export
repair_attributes.grouped_df <- function(x, code, ..., pipe = "base", one_liner = FALSE) {
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = c("row.names", "groups"),
    idiomatic_class = c("grouped_df", "tbl_df", "tbl", "data.frame"),
    one_liner = one_liner
  )
}

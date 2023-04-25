# FIXME: optionally construct pointer

constructors$data.table <- new.env()

#' Constructive options for class 'data.table'
#'
#' These options will be used on objects of class 'data.table'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"data.table"` (default): Wrap the column definitions in a `data.table()` call.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"list"` : Use `list()` and treat the class as a regular attribute.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_data.table>
#' @export
opts_data.table <- function(constructor = c("data.table", "next", "list"), ...) {
  combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  constructive_options("data.table", constructor = constructor)
}

#' @export
construct_raw.data.table <- function(x, ...) {
  opts <- fetch_opts("data.table", ...)
  if (is_corrupted_data.table(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$data.table[[opts$constructor]]
  constructor(x, ...)
}

#' @export
is_corrupted_data.table <- function(x) {
  is_corrupted_data.frame(x)
}

constructors$data.table$list <- function(x, ...) {
  construct_raw.list(x, ...)
}

constructors$data.table$data.table <- function(x, ...) {
  code <- construct_apply(x, fun = "data.table::data.table", ...)
  repair_attributes.data.table(x, code, ...)
}

#' @export
repair_attributes.data.table <- function(x, code, ..., pipe = "base") {
  ignore <- c("row.names", ".internal.selfref")
  if (identical(names(x), character())) ignore <- c(ignore, "names")
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = ignore,
    idiomatic_class = c("data.table", "data.frame")
  )
}

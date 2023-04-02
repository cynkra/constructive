#' Constructive options for class 'data.table'
#'
#' These options will be used on objects of class 'data.table'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"data.table"` (default): Wrap the column definitions in a `data.table()` call.
#' * `"list"` : Use `list()` and treat the class as a regular attribute.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_data.table>
#' @export
opts_data.table <- function(constructor = c("data.table", "list"), ...) {
  combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  constructive_options("data.table", constructor = constructor)
}

#' @export
construct_idiomatic.data.table <- function(x, ...) {
  opts <- fetch_opts("data.table", ...)
  if (opts$constructor == "list") {
    return(construct_idiomatic.list(x, ...))
  }
  construct_apply(x, fun = "data.table::data.table", ...)
}

#' @export
repair_attributes.data.table <- function(x, code, ..., pipe = "base") {
  opts <- fetch_opts("data.table", ...)
  if (opts$constructor == "list") {
    return(repair_attributes.default(x, code, ..., pipe = pipe))
  }
  ignore <- c("row.names", ".internal.selfref")
  if (identical(names(x), character())) ignore <- c(ignore, "names")
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = ignore,
    idiomatic_class = c("data.table", "data.frame")
  )
}

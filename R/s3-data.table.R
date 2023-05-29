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
#' @param selfref Boolean. Whether to include the `.internal.selfref` attribute. It's
#'   probably not useful, hence the default, `waldo::compare()` is used to assess the output
#'   fidelity and doesn't check it, but if you really need to generate code that builds
#'   an object `identical()` to the input you'll need to set this to `TRUE`.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_data.table>
#' @export
opts_data.table <- function(constructor = c("data.table", "next", "list"), ..., selfref = FALSE) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "data.table"),
    ellipsis::check_dots_empty()
  )
  .cstr_options("data.table", constructor = constructor, selfref = selfref)
}

#' @export
.cstr_construct.data.table <- function(x, ...) {
  opts <- .cstr_fetch_opts("data.table", ...)
  if (is_corrupted_data.table(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$data.table[[opts$constructor]]
  constructor(x, selfref = opts$selfref, ...)
}

is_corrupted_data.table <- function(x) {
  is_corrupted_data.frame(x)
}

constructors$data.table$list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

constructors$data.table$data.table <- function(x, selfref, ...) {
  key <- attr(x, "sorted")
  if (!is.null(key)) {
    args <- c(x, key = key)
  } else {
    args <- x
  }
  code <- .cstr_apply(args, fun = "data.table::data.table", ...)
  repair_attributes_data.table(x, code, ..., selfref = selfref)
}

repair_attributes_data.table <- function(x, code, ..., pipe = "base", selfref = FALSE) {
  ignore <- c("row.names", "sorted")
  if (!selfref) ignore <- c(ignore, ".internal.selfref")
  if (identical(names(x), character())) ignore <- c(ignore, "names")
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = ignore,
    idiomatic_class = c("data.table", "data.frame")
  )
}

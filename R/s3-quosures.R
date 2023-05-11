constructors$quosures <- new.env()

#' Constructive options for class 'quosures'
#'
#' These options will be used on objects of class 'quosures'.
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"as_quosures"` (default): Build the object using a `as_quosures()` call on a
#' character vector.
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
opts_quosures <- function(constructor = c("new_quosures", "next", "list"), ..., origin = "1970-01-01") {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  .cstr_options("quosures", constructor = constructor, origin = origin)
}

#' @export
construct_raw.quosures <- function(x, ...) {
  opts <- .cstr_fetch_opts("quosures", ...)
  if (is_corrupted_quosures(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$quosures[[opts$constructor]]
  constructor(x, ...)
}

#' @export
is_corrupted_quosures <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$quosures$new_quosures <- function(x, ...) {
  x_list <- unclass(x)
  # remove names if "" so we avoid repairing the names
  if (all(names(x) == "")) names(x_list) <- NULL
  list_code <- construct_raw.list(x_list, ...)
  code <- .cstr_apply(list(list_code), "rlang::new_quosures", language = TRUE, ...)
  repair_attributes.quosures(x, code, ...)
}

#' @export
constructors$quosures$list <- function(x, ...) {
  construct_raw.list(x, ...)
}

#' @export
repair_attributes.quosures <- function(x, code, ...) {
  repair_attributes_impl(
    x, code, ...,
    idiomatic_class = c("quosures", "list"),
    ignore = if (all(names(x) == "")) "names"
  )
}

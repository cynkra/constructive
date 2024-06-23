#' Constructive options for class 'quosures'
#'
#' These options will be used on objects of class 'quosures'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as_quosures"` (default): Build the object using a `as_quosures()` call on a
#' character vector.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"list"` : We define as an list object and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_quosures>
#' @export
opts_quosures <- function(constructor = c("new_quosures", "next", "list"), ...) {
  .cstr_options("quosures", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct quosures
.cstr_construct.quosures <- function(x, ...) {
  opts <- list(...)$opts$quosures %||% opts_quosures()
  if (is_corrupted_quosures(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.quosures", structure(NA, class = opts$constructor))
}

is_corrupted_quosures <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.quosures new_quosures
.cstr_construct.quosures.new_quosures <- function(x, ...) {
  x_list <- unclass(x)
  # remove names if "" so we avoid repairing the names
  if (all(names(x) == "")) names(x_list) <- NULL
  list_code <- .cstr_construct.list(x_list, ...)
  code <- .cstr_apply(list(list_code), "rlang::new_quosures", recurse = FALSE, ...)
  repair_attributes_quosures(x, code, ...)
}

#' @export
#' @method .cstr_construct.quosures list
.cstr_construct.quosures.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_quosures <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("quosures", "list"),
    ignore = if (all(names(x) == "")) "names"
  )
}

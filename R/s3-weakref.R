#' Constructive options for the class `weakref`
#'
#' These options will be used on objects of type `weakref`. `weakref` objects
#' are rarely encountered and there is no base R function to create them. However
#' \pkg{rlang} has a `new_weakref` function that we can use.
#'
#' @param constructor String. Name of the constructor.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_array>
#' @export
opts_weakref <- function(constructor = c("new_weakref"), ...) {
  .cstr_options("weakref", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct weakref
.cstr_construct.weakref <- function(x, ...) {
  opts <- list(...)$opts$weakref %||% opts_weakref()
  if (is_corrupted_weakref(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.weakref", structure(NA, class = opts$constructor))
}

is_corrupted_weakref <- function(x) {
  !rlang::is_weakref(x)
}

#' @export
#' @method .cstr_construct.weakref new_weakref
.cstr_construct.weakref.new_weakref <- function(x, ...) {
  args <- list(rlang::wref_key(x))
  # assigned this way so no element is added if NULL
  args$value <- rlang::wref_value(x)
  code <- .cstr_apply(args, "rlang::new_weakref", ...)
  repair_attributes_weakref(x, code, ...)
}

repair_attributes_weakref <- function(x, code, ...) {
  # FIXME do these need any repair ?
  code
}

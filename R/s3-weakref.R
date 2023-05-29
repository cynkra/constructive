constructors$weakref <- new.env()

#' Constructive options for the class `weakref`
#'
#' These options will be used on objects of type `weakref`. `weakref` objects
#' are rarely encountered and there is no base R function to create them. However
#' {rlang} has a `new_weakref` function that we can use.
#'
#' @param constructor String. Name of the constructor.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_array>
#' @export
opts_weakref <- function(constructor = c("new_weakref"), ...) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "weakref"),
    ellipsis::check_dots_empty()
  )
  .cstr_options("weakref", constructor = constructor)
}

#' @export
.cstr_construct.weakref <- function(x, ...) {
  opts <- .cstr_fetch_opts("weakref", ...)
  if (is_corrupted_weakref(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$weakref[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_weakref <- function(x) {
  !rlang::is_weakref(x)
}

constructors$weakref$new_weakref <- function(x, ...) {
  args <- list(rlang::wref_key(x))
  # assigned this way so no element is added if NULL
  args$value <- rlang::wref_value(x)
  code <- .cstr_apply(args, "rlang::new_weakref", ...)
  repair_attributes_weakref(x, code, ...)
}

repair_attributes_weakref <- function(x, code, ...) {
  # FIXME do these need any reparation ?
  code
}

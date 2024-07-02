#' Constructive options for class 'CLASS'
#'
#' These options will be used on objects of class 'CLASS'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"CONSTRUCTOR"` (default): We build the object using `CONSTRUCTOR()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_CLASS>
#' @export
opts_CLASS <- function(constructor = c("CONSTRUCTOR", "next"), ...) {
  .cstr_options("CLASS", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct CLASS
.cstr_construct.CLASS <- function(x, ...) {
  opts <- list(...)$opts$CLASS %||% opts_CLASS()
  if (is_corrupted_CLASS(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.CLASS", structure(NA, class = opts$constructor))
}

is_corrupted_CLASS <- function(x) {
  FALSE
}

#' @export
#' @method .cstr_construct.CLASS CONSTRUCTOR
.cstr_construct.CLASS.CONSTRUCTOR <- function(x, ...) {
  opts <- list(...)$opts$CLASS %||% opts_CLASS()
  args <- ...
  code <- .cstr_apply(args, fun = "CONSTRUCTOR", ...)
  repair_attributes_CLASS(x, code, ...)
}

repair_attributes_CLASS <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("CLASS")
  )
}

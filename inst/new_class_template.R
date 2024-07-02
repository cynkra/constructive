#' @importFrom constructive .cstr_options .cstr_construct .cstr_apply .cstr_repair_attributes
NULL

#' Constructive options for class '.CLASS1.'
#'
#' These options will be used on objects of class '.CLASS1.'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `".CONSTRUCTOR."` (default): We build the object using `.PKG::CONSTRUCTOR.()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_.CLASS1.>
#' @export
opts_.CLASS1. <- function(constructor = c(".CONSTRUCTOR.", "next"), ...) {
  .cstr_options(".CLASS1.", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct .CLASS1.
.cstr_construct..CLASS1. <- function(x, ...) {
  opts <- list(...)$opts$.CLASS1. %||% opts_.CLASS1.()
  if (is_corrupted_.CLASS1.(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct..CLASS1.", structure(NA, class = opts$constructor))
}

is_corrupted_.CLASS1. <- function(x) {
  FALSE
}

#' @export
#' @method .cstr_construct..CLASS1. .CONSTRUCTOR.
.cstr_construct..CLASS1...CONSTRUCTOR. <- function(x, ...) {
  # opts <- list(...)$opts$.CLASS1. %||% opts_.CLASS1.()
  args <- list()
  code <- .cstr_apply(args, fun = ".PKG::CONSTRUCTOR.", ...)
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = .CLASS.
  )
}

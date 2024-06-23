#' Constructive options for class 'classRepresentation'
#'
#' These options will be used on objects of class 'classRepresentation'.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_classRepresentation>
#' @export
opts_classRepresentation <- function(constructor = c("getClassDef"), ...) {
  .cstr_options("classRepresentation", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct classRepresentation
.cstr_construct.classRepresentation <- function(x, ...) {
  opts <- list(...)$opts$classRepresentation %||% opts_classRepresentation()
  if (is_corrupted_classRepresentation(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.classRepresentation", structure(NA, class = opts$constructor))
}

is_corrupted_classRepresentation <- function(x) {
  # FIXME
  !isS4(x)
}

#' @export
#' @method .cstr_construct.classRepresentation getClassDef
.cstr_construct.classRepresentation.getClassDef <- function(x, env, ...) {
  # FIXME: what about multiple classes ? is this considered corrupted in S4 ?
  cl <- x@className
  attr(cl, "package") <- NULL
  pkg <- x@package
  code <- .cstr_apply(list(cl, package = pkg), fun = "getClassDef", env = env, ...)
  repair_attributes_S4(x, code, env = env, ...)
}

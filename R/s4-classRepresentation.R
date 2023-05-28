constructors$classRepresentation <- new.env()

#' Constructive options for class 'classRepresentation'
#'
#' These options will be used on objects of class 'classRepresentation'.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @return An object of class <constructive_options/constructive_options_classRepresentation>
#' @export
opts_classRepresentation <- function(constructor = c("getClassDef"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  .cstr_options("classRepresentation", constructor = constructor)
}

#' @export
.cstr_construct.classRepresentation <- function(x, ...) {
  opts <- .cstr_fetch_opts("classRepresentation", ...)
  if (is_corrupted_classRepresentation(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$classRepresentation[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_classRepresentation <- function(x) {
  # FIXME
  !isS4(x)
}

constructors$classRepresentation$getClassDef <- function(x, env, ...) {
  # FIXME: what about multiple classes ? is this considered corrupted in S4 ?
  cl <- x@className
  attr(cl, "package") <- NULL
  pkg <- x@package
  code <- .cstr_apply(list(cl, package = pkg), fun = "getClassDef", env = env, ...)
  repair_attributes.S4(x, code, env = env, ...)
}

constructors$classGeneratorFunction <- new.env()

#' Constructive options for class 'classGeneratorFunction'
#'
#' These options will be used on objects of class 'classGeneratorFunction'.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_classGeneratorFunction>
#' @export
opts_classGeneratorFunction <- function(constructor = c("setClass"), ...) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "classGeneratorFunction"),
    check_dots_empty()
  )
  .cstr_options("classGeneratorFunction", constructor = constructor)
}

#' @export
.cstr_construct.classGeneratorFunction <- function(x, ...) {
  opts <- .cstr_fetch_opts("classGeneratorFunction", ...)
  if (is_corrupted_classGeneratorFunction(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$classGeneratorFunction[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_classGeneratorFunction <- function(x) {
  # FIXME
  !isS4(x)
}

constructors$classGeneratorFunction$setClass <- function(x, env, ...) {
  # FIXME: what about multiple classes ? is this considered corrupted in S4 ?
  cl <- x@className
  if (
    attr(cl, "package") == environmentName(env) ||
    (identical(env, .GlobalEnv) && attr(cl, "package") == ".GlobalEnv")) {
    attr(cl, "package") <- NULL
  }
  code <- .cstr_apply(list(cl, slots = getSlots(cl)), fun = "setClass", env = env, ...)
  repair_attributes_S4(x, code, env = env, ...)
}

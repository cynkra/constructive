#' Constructive options for class 'classGeneratorFunction'
#'
#' These options will be used on objects of class 'classGeneratorFunction'.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_classGeneratorFunction>
#' @export
opts_classGeneratorFunction <- function(constructor = c("setClass"), ...) {
  .cstr_options("classGeneratorFunction", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct classGeneratorFunction
.cstr_construct.classGeneratorFunction <- function(x, ...) {
  opts <- list(...)$opts$classGeneratorFunction %||% opts_classGeneratorFunction()
  if (is_corrupted_classGeneratorFunction(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.classGeneratorFunction", structure(NA, class = opts$constructor))
}

is_corrupted_classGeneratorFunction <- function(x) {
  # FIXME
  !isS4(x)
}

#' @export
#' @method .cstr_construct.classGeneratorFunction setClass
.cstr_construct.classGeneratorFunction.setClass <- function(x, env, ...) {
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

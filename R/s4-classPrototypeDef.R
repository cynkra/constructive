#' Constructive options for class 'classPrototypeDef'
#'
#' These options will be used on objects of class 'classPrototypeDef'.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_classPrototypeDef>
#' @export
opts_classPrototypeDef <- function(constructor = c("prototype"), ...) {
  .cstr_options("classPrototypeDef", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct classPrototypeDef
.cstr_construct.classPrototypeDef <- function(x, ...) {
  opts <- list(...)$opts$classPrototypeDef %||% opts_classPrototypeDef()
  if (is_corrupted_classPrototypeDef(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.classPrototypeDef", structure(NA, class = opts$constructor))
}

is_corrupted_classPrototypeDef <- function(x) {
  # FIXME
  !isS4(x)
}

#' @export
#' @method .cstr_construct.classPrototypeDef prototype
.cstr_construct.classPrototypeDef.prototype <- function(x, env, ...) {
  object <- x@object
  slots <- getSlots(x)
  attrs <- attributes(object)[slots]
  attributes(object)[slots] <- NULL
  code <- .cstr_apply(c(list(object), attrs), fun = "prototype", env = env, ...)
  .cstr_repair_attributes(
    x, code, env = env, ...,
    ignore = names(getSlots(class(x))),
    idiomatic_class = class(x),
    flag_s4 = FALSE
  )
}

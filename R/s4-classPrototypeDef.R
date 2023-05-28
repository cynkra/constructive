constructors$classPrototypeDef <- new.env()

#' Constructive options for class 'classPrototypeDef'
#'
#' These options will be used on objects of class 'classPrototypeDef'.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @return An object of class <constructive_options/constructive_options_classPrototypeDef>
#' @export
opts_classPrototypeDef <- function(constructor = c("prototype"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  .cstr_options("classPrototypeDef", constructor = constructor)
}

#' @export
.cstr_construct.classPrototypeDef <- function(x, ...) {
  opts <- .cstr_fetch_opts("classPrototypeDef", ...)
  if (is_corrupted_classPrototypeDef(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$classPrototypeDef[[opts$constructor]]
  constructor(x, ...)
}

#' @export
is_corrupted_classPrototypeDef <- function(x) {
  # FIXME
  !isS4(x)
}

constructors$classPrototypeDef$prototype <- function(x, env, ...) {
  object <- x@object
  slots <- getSlots(x)
  attrs <- attributes(object)[slots]
  attributes(object)[slots] <- NULL
  args <-
  code <- .cstr_apply(c(list(object), attrs), fun = "prototype", env = env, ...)
  repair_attributes.S4(x, code, env = env, ...)
}

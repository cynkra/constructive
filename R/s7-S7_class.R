#' Constructive options for class 'S7_class'
#'
#' These options will be used on objects of class 'S7_class'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"new_class"` (default): We build the object using `S7::new_class()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_S7_class>
#' @export
opts_S7_class <- function(constructor = c("new_class", "next"), ...) {
  constructive::.cstr_options("S7_class", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.S7_class <- function(x, ...) {
  opts <- list(...)$opts$S7_class %||% opts_S7_class()
  if (is_corrupted_S7_class(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.S7_class", structure(NA, class = opts$constructor))
}

is_corrupted_S7_class <- function(x) {
  FALSE
}

#' @export
.cstr_construct.S7_class.new_class <- function(x, ...) {
  if (identical(x, S7::S7_object)) return("S7::S7_object")
  # opts <- list(...)$opts$S7_class %||% opts_S7_class()
  attr_names <- c("name", "parent", "properties", "abstract", "constructor")
  args <- attributes(x)[attr_names]
  # let's not name the first arg (`name`)
  names(args)[[1]] <- ""
  # use defaults whenever possible
  if (!args$abstract) args$abstract <- NULL
  if (is_default_s7_class_constructor(args$constructor)) {
    args$constructor <- NULL
  }
  args$properties <- simplify_s7_class_properties(args$properties)

  code <- constructive::.cstr_apply(args, fun = "S7::new_class", ...)

  # repair environment
  code <- .cstr_wrap(code, fun = "")
  envir_code <- .cstr_apply(list(environment(x)), "(`environment<-`)", ...)
  code <- .cstr_pipe(code, envir_code, ...)

  constructive::.cstr_repair_attributes(
    x, code, ...,
    ignore = attr_names,
    idiomatic_class = c("S7_class", "S7_object")
  )
}

is_default_s7_class_constructor <- function(constructor) {
  # FIXME: in ellmer for instance the default constructor is built with `new_object()`
  # while with our version it's `S7::new_object()`, also the default constructor's env is
  # the global env if we eval it there, so we're probably better off always constructing it
  return(FALSE)

  # might be improved but from doc and toying around I gather that custom
  # constructors (assuming no corruption)  have a .data formal and default ones
  # haven't
  formalArgs(constructor)[[1]] != ".data"
}

simplify_s7_class_properties <- function(properties) {
  lapply(properties, function(x) {
    all_optional_args_are_null <-
      is.null(x$getter) && is.null(x$setter) && is.null(x$validator) &&
      is.null(x$default)
    if (all_optional_args_are_null) {
      x$class
    } else {
      x
    }
  })
}

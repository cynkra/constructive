#' Constructive options for class 'S7_object'
#'
#' These options will be used on objects of class 'S7_object'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"S7_object"` (default): We build the object using `S7::S7_object()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_S7_object>
#' @export
opts_S7_object <- function(constructor = c("S7_object", "next"), ...) {
  constructive::.cstr_options("S7_object", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.S7_object <- function(x, ...) {
  opts <- list(...)$opts$S7_object %||% opts_S7_object()
  if (is_corrupted_S7_object(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.S7_object", structure(NA, class = opts$constructor))
}

is_corrupted_S7_object <- function(x) {
  FALSE
}


#' @export
.cstr_construct.S7_object.new_class<- function(x, ...) {
  constructor <- attr(attr(x, "S7_class"), "constructor")
  we_can_use_compact_syntax <-
    !identical(x, S7::S7_object()) && is_default_s7_class_constructor(constructor)
  if (!we_can_use_compact_syntax) return(.cstr_construct.S7_object.S7_object(x, ...))
  fun_code <- .cstr_construct(attr(x, "S7_class"), ...)
  args <- attributes(x)
  args$class <- NULL
  args$S7_class <- NULL
  code <- constructive::.cstr_apply(args, fun = fun_code, ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    ignore = setdiff(names(attributes(x)), "class"),
    idiomatic_class = c(attr(attr(x, "S7_class"), "name"), "S7_object")
  )
}

#' @export
.cstr_construct.S7_object.S7_object <- function(x, ...) {
  code <- "S7::S7_object()"
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "S7_object"
  )
}


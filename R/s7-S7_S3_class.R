#' Constructive options for class 'S7_S3_class'
#'
#' These options will be used on objects of class 'S7_S3_class'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"new_S3_class"` (default): We build the object using `S7::new_S3_class()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_S7_S3_class>
#' @export
opts_S7_S3_class <- function(constructor = c("new_S3_class", "next"), ...) {
  constructive::.cstr_options("S7_S3_class", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.S7_S3_class <- function(x, ...) {
  opts <- list(...)$opts$S7_S3_class %||% opts_S7_S3_class()
  if (is_corrupted_S7_S3_class(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.S7_S3_class", structure(NA, class = opts$constructor))
}

is_corrupted_S7_S3_class <- function(x) {
  FALSE
}

#' @export
.cstr_construct.S7_S3_class.new_S3_class <- function(x, ...) {
  # opts <- list(...)$opts$S7_S3_class %||% opts_S7_S3_class()
  args <- list(x$class)
  if (!is.null(x$validator)) args$validator <- x$validator
  constructor_is_default <-
    identical(topenv(environment(x$constructor)), asNamespace("S7"))
  if (!constructor_is_default) args$constructor <- x$constructor
  code <- constructive::.cstr_apply(args, fun = "S7::new_S3_class", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "S7_S3_class"
  )
}

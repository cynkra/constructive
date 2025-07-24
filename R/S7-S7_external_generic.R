#' Constructive options for class 'S7_external_generic'
#'
#' These options will be used on objects of class 'S7_external_generic'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"new_external_generic"` (default): We build the object using `S7::new_external_generic()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_S7_external_generic>
#' @export
opts_S7_external_generic <- function(constructor = c("new_external_generic", "next"), ...) {
  constructive::.cstr_options("S7_external_generic", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.S7_external_generic <- function(x, ...) {
  opts <- list(...)$opts$S7_external_generic %||% opts_S7_external_generic()
  if (is_corrupted_S7_external_generic(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.S7_external_generic", structure(NA, class = opts$constructor))
}

is_corrupted_S7_external_generic <- function(x) {
  FALSE
}

#' @export
.cstr_construct.S7_external_generic.new_external_generic <- function(x, ...) {
  # opts <- list(...)$opts$S7_external_generic %||% opts_S7_external_generic()
  args <- list(
    x$package,
    x$name,
    dispatch_args = x$dispatch_args)
  args$version <- x$version
  code <- constructive::.cstr_apply(args, fun = "S7::new_external_generic", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "S7_external_generic"
  )
}

#' Constructive options for class 'S7_property'
#'
#' These options will be used on objects of class 'S7_property'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"new_property"` (default): We build the object using `S7::new_property()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_S7_property>
#' @export
opts_S7_property <- function(constructor = c("new_property", "next"), ...) {
  constructive::.cstr_options("S7_property", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.S7_property <- function(x, ...) {
  opts <- list(...)$opts$S7_property %||% opts_S7_property()
  if (is_corrupted_S7_property(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.S7_property", structure(NA, class = opts$constructor))
}

is_corrupted_S7_property <- function(x) {
  FALSE
}

#' @export
.cstr_construct.S7_property.new_property <- function(x, ...) {
  # opts <- list(...)$opts$S7_property %||% opts_S7_property()
  args <- list()
  if (!identical(x$class, S7::class_any)) args[[1]] <- x$class
  if (!is.null(x$getter)) args$getter <- x$getter
  if (!is.null(x$setter)) args$setter <- x$setter
  if (!is.null(x$validator)) args$validator <- x$validator
  if (!is.null(x$default)) args$default <- x$default
  if (!is.null(x$name)) args$name <- x$name

  code <- constructive::.cstr_apply(args, fun = "S7::new_property", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "S7_property"
  )
}

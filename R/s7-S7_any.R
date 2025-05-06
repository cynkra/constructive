#' Constructive options for class 'S7_any'
#'
#' These options will be used on objects of class 'S7_any'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"class_any"` (default): We build the object using `S7::class_any()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_S7_any>
#' @export
opts_S7_any <- function(constructor = c("class_any", "next"), ...) {
  constructive::.cstr_options("S7_any", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.S7_any <- function(x, ...) {
  opts <- list(...)$opts$S7_any %||% opts_S7_any()
  if (is_corrupted_S7_any(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.S7_any", structure(NA, class = opts$constructor))
}

is_corrupted_S7_any <- function(x) {
  FALSE
}

#' @export
.cstr_construct.S7_any.class_any <- function(x, ...) {
  # opts <- list(...)$opts$S7_any %||% opts_S7_any()
  code <- "S7::class_any"
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "S7_any"
  )
}

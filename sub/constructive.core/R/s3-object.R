#' Constructive options for class 'object'
#'
#' These options will be used on objects of class 'object'. The 'object' type
#' is particular in that it is an internal type in base R, but can only be
#' produced at time of writing through the 'S7' package. Well this is not
#' completely true since those can be built from S4 objects that we remove
#' the S4 flag from by using `asS3(x, complete = FALSE)` but we don't propose
#' this for now.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"S7_object"` (default): We build the object using `S7::S7_object()`.
#'   At the time of writing, this is currently the only way to create these objects.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_object>
#' @export
opts_object <- function(constructor = c("prototype", "S7_object"), ...) {
  constructive::.cstr_options("object", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.object <- function(x, ...) {
  opts <- list(...)$opts$object %||% opts_object()
  if (is_corrupted_object(x)) return(NextMethod())
  UseMethod(".cstr_construct.object", structure(NA, class = opts$constructor))
}

is_corrupted_object <- function(x) {
  typeof(x) != "object"
}

#' @export
.cstr_construct.object.S7_object <- function(x, ...) {
  code <- "S7::S7_object()"
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "S7_object"
  )
}

#' @export
.cstr_construct.object.prototype <- function(x, ...) {
  "getClass(\"S4\")@prototype"
  code <- .cstr_pipe("getClass(\"S4\")@prototype", "asS3(complete = FALSE)")
  .cstr_repair_attributes(
    x, code, ...,
    flag_s4 = FALSE
  )
}

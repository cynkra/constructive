#' Constructive options for type 'externalptr'
#'
#' These options will be used on objects of type 'externalptr'. By default this
#' function is useless as nothing can be set, this is provided in case users wan
#' to extend the method with other constructors.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"default"` : We use a special function from the constructive
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_externalptr>
#' @export
opts_externalptr  <- function(constructor = c("default"), ...) {
  .cstr_options("externalptr", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct externalptr
.cstr_construct.externalptr <- function(x, ...) {
  opts <- list(...)$opts$externalptr %||% opts_externalptr()
  if (is_corrupted_externalptr (x)) return(NextMethod())
  UseMethod(".cstr_construct.externalptr", structure(NA, class = opts$constructor))
}

is_corrupted_externalptr <- function(x) {
  typeof(x) != "externalptr"
}

#' @export
#' @method .cstr_construct.externalptr default
.cstr_construct.externalptr.default <- function(x, ...) {
  code <- sprintf('constructive::.xptr("%s")', external_pointer_address(x))
  repair_attributes_externalptr(x, code, ...)
}

repair_attributes_externalptr <- function(x, code, ...) {
  .cstr_repair_attributes(x, code, ...)
}

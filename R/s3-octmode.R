#' Constructive options for class 'octmode'
#'
#' These options will be used on objects of class 'octmode'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as.octmode"` (default): We build the object using `as.octmode()`
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @param integer Whether to use `as.octmode()` on integer rather than character
#' @return An object of class <constructive_options/constructive_options_octmode>
#' @export
opts_octmode <- function(constructor = c("as.octmode", "next"), ..., integer = FALSE) {
  abort_not_boolean(integer)
  .cstr_options("octmode", constructor = constructor[[1]], ..., integer = integer)
}

#' @export
#' @method .cstr_construct octmode
.cstr_construct.octmode <- function(x, ...) {
  opts <- list(...)$opts$octmode %||% opts_octmode()
  if (is_corrupted_octmode(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.octmode", structure(NA, class = opts$constructor))
}

is_corrupted_octmode <- function(x) {
  !(is.integer(x))
}

#' @export
#' @method .cstr_construct.octmode as.octmode
.cstr_construct.octmode.as.octmode <- function(x, ...) {
  opts <- list(...)$opts$octmode %||% opts_octmode()
  # we let attributes to the repair step
  x_bkp <- x
  attributes(x) <- NULL
  if (opts$integer) {
    code <- .cstr_apply(list(unclass(x)), "as.octmode", ...)
  } else {
    code <- .cstr_apply(list(format.octmode(x)), "as.octmode", ...)
  }
  repair_attributes_octmode(x_bkp, code, ...)
}

repair_attributes_octmode <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe = pipe,
    idiomatic_class = "octmode",
    ...
  )
}

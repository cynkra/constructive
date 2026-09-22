#' Constructive options for class 'sparseVector'
#'
#' These options will be used on objects of class 'sparseVector' from the
#' 'Matrix' package, i.e. objects of classes such as 'dsparseVector',
#' 'isparseVector', 'lsparseVector', 'nsparseVector' or 'zsparseVector'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"sparseVector"` (default): We build the object using
#'   `Matrix::sparseVector()`.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_sparseVector>
#' @export
opts_sparseVector <- function(constructor = c("sparseVector", "next"), ...) {
  .cstr_options("sparseVector", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct sparseVector
.cstr_construct.sparseVector <- function(x, ...) {
  opts <- list(...)$opts$sparseVector %||% opts_sparseVector()
  if (is_corrupted_sparseVector(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.sparseVector", structure(NA, class = opts$constructor))
}

is_corrupted_sparseVector <- function(x) {
  typeof(x) != "S4" ||
    !identical_slots(x, do.call(Matrix::sparseVector, sparseVector_args(x)))
}

#' @export
#' @method .cstr_construct.sparseVector sparseVector
.cstr_construct.sparseVector.sparseVector <- function(x, ...) {
  code <- .cstr_apply(sparseVector_args(x), "Matrix::sparseVector", ...)
  repair_attributes_sparseVector(x, code, ...)
}

repair_attributes_sparseVector <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = methods::slotNames(x),
    idiomatic_class = class(x),
    flag_s4 = FALSE
  )
}

sparseVector_args <- function(x) {
  # "nsparseVector" objects have no `x` slot
  args <- if (methods::.hasSlot(x, "x")) list(x = x@x) else list()
  c(args, list(i = x@i, length = x@length))
}

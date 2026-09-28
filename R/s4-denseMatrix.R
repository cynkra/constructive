#' Constructive options for class 'denseMatrix'
#'
#' These options will be used on objects of class 'denseMatrix' from the
#' 'Matrix' package, i.e. objects of classes such as 'dgeMatrix', 'lgeMatrix',
#' 'dsyMatrix' or 'dtrMatrix'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"Matrix"` (default): We build the object using `Matrix::Matrix()` on a
#'   base matrix, with `sparse = FALSE`. `Matrix::Matrix()` detects symmetric
#'   and triangular matrices so if this cannot reproduce the object exactly
#'   (e.g. a general matrix with symmetric content, packed storage, cached
#'   factorizations) we fall back to the next constructor.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_denseMatrix>
#' @export
opts_denseMatrix <- function(constructor = c("Matrix", "next"), ...) {
  .cstr_options("denseMatrix", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct denseMatrix
.cstr_construct.denseMatrix <- function(x, ...) {
  opts <- list(...)$opts$denseMatrix %||% opts_denseMatrix()
  if (is_corrupted_denseMatrix(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.denseMatrix", structure(NA, class = opts$constructor))
}

is_corrupted_denseMatrix <- function(x) {
  typeof(x) != "S4" ||
    !identical_slots(x, Matrix::Matrix(methods::as(x, "matrix"), sparse = FALSE))
}

#' @export
#' @method .cstr_construct.denseMatrix Matrix
.cstr_construct.denseMatrix.Matrix <- function(x, ...) {
  args <- list(methods::as(x, "matrix"), sparse = FALSE)
  code <- .cstr_apply(args, "Matrix::Matrix", ...)
  repair_attributes_denseMatrix(x, code, ...)
}

repair_attributes_denseMatrix <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = methods::slotNames(x),
    idiomatic_class = class(x),
    flag_s4 = FALSE
  )
}

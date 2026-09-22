#' Constructive options for class 'diagonalMatrix'
#'
#' These options will be used on objects of class 'diagonalMatrix' from the
#' 'Matrix' package, i.e. objects of classes 'ddiMatrix' or 'ldiMatrix'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"Diagonal"` (default): We build the object using `Matrix::Diagonal()`.
#'   If this cannot reproduce the object exactly (e.g. different row and column
#'   names) we fall back to the next constructor.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_diagonalMatrix>
#' @export
opts_diagonalMatrix <- function(constructor = c("Diagonal", "next"), ...) {
  .cstr_options("diagonalMatrix", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct diagonalMatrix
.cstr_construct.diagonalMatrix <- function(x, ...) {
  opts <- list(...)$opts$diagonalMatrix %||% opts_diagonalMatrix()
  if (is_corrupted_diagonalMatrix(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.diagonalMatrix", structure(NA, class = opts$constructor))
}

is_corrupted_diagonalMatrix <- function(x) {
  typeof(x) != "S4" ||
    !identical_slots(x, do.call(Matrix::Diagonal, diagonalMatrix_args(x)))
}

#' @export
#' @method .cstr_construct.diagonalMatrix Diagonal
.cstr_construct.diagonalMatrix.Diagonal <- function(x, ...) {
  code <- .cstr_apply(diagonalMatrix_args(x), "Matrix::Diagonal", ...)
  repair_attributes_diagonalMatrix(x, code, ...)
}

repair_attributes_diagonalMatrix <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = methods::slotNames(x),
    idiomatic_class = class(x),
    flag_s4 = FALSE
  )
}

diagonalMatrix_args <- function(x) {
  # a double prints more nicely than an integer and `Diagonal()` accepts both
  args <- list(as.double(x@Dim[[1]]))
  # unit diagonal matrices have an empty `x` slot, `Diagonal(n)` builds a
  # "ddiMatrix" and `Diagonal(n, TRUE)` builds a "ldiMatrix"
  if (x@diag == "N") {
    args$x <- x@x
  } else if (methods::is(x, "ldiMatrix")) {
    args$x <- TRUE
  }
  if (!is.null(x@Dimnames[[1]])) args$names <- x@Dimnames[[1]]
  args
}

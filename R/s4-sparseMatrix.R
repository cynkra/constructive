#' Constructive options for class 'sparseMatrix'
#'
#' These options will be used on objects of class 'sparseMatrix' from the
#' 'Matrix' package in compressed column, compressed row or triplet form, i.e.
#' objects of classes such as 'dgCMatrix', 'dgRMatrix', 'dgTMatrix',
#' 'lgCMatrix', 'ngCMatrix', 'dsCMatrix' or 'dtCMatrix'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"sparseMatrix"` (default): We build the object using
#'   `Matrix::sparseMatrix()` on the (1-based) row and column indices of the
#'   stored entries. If this cannot reproduce the object exactly (e.g. cached
#'   factorizations, unit triangular matrices) we fall back to the next
#'   constructor.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_sparseMatrix>
#' @export
opts_sparseMatrix <- function(constructor = c("sparseMatrix", "next"), ...) {
  .cstr_options("sparseMatrix", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct sparseMatrix
.cstr_construct.sparseMatrix <- function(x, ...) {
  opts <- list(...)$opts$sparseMatrix %||% opts_sparseMatrix()
  if (is_corrupted_sparseMatrix(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.sparseMatrix", structure(NA, class = opts$constructor))
}

is_corrupted_sparseMatrix <- function(x) {
  # "diagonalMatrix" and "indMatrix" also extend "sparseMatrix", but they have
  # a different structure and are handled by their own methods
  if (typeof(x) != "S4" || is.null(sparseMatrix_repr(x))) return(TRUE)
  !identical_slots(x, do.call(Matrix::sparseMatrix, sparseMatrix_args(x)))
}

#' @export
#' @method .cstr_construct.sparseMatrix sparseMatrix
.cstr_construct.sparseMatrix.sparseMatrix <- function(x, ...) {
  code <- .cstr_apply(sparseMatrix_args(x), "Matrix::sparseMatrix", ...)
  repair_attributes_sparseMatrix(x, code, ...)
}

repair_attributes_sparseMatrix <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = methods::slotNames(x),
    idiomatic_class = class(x),
    flag_s4 = FALSE
  )
}

sparseMatrix_repr <- function(x) {
  if (methods::is(x, "CsparseMatrix")) return("C")
  if (methods::is(x, "RsparseMatrix")) return("R")
  if (methods::is(x, "TsparseMatrix")) return("T")
  NULL
}

sparseMatrix_args <- function(x) {
  repr <- sparseMatrix_repr(x)
  # compressed indices are expanded to 1-based triplets, we use doubles rather
  # than integers for indices and dimensions as they print more nicely
  i <- if (repr == "R") rep(seq_len(x@Dim[[1]]), diff(x@p)) else x@i + 1
  j <- if (repr == "C") rep(seq_len(x@Dim[[2]]), diff(x@p)) else x@j + 1
  args <- list(i = as.double(i), j = as.double(j))
  # "pattern" matrices ("ngCMatrix"...) have no `x` slot
  if (methods::.hasSlot(x, "x")) args$x <- x@x
  args$dims <- as.double(x@Dim)
  if (!identical(x@Dimnames, list(NULL, NULL))) args$dimnames <- x@Dimnames
  if (methods::is(x, "symmetricMatrix")) args$symmetric <- TRUE
  if (methods::is(x, "triangularMatrix")) args$triangular <- TRUE
  if (repr != "C") args$repr <- repr
  args
}

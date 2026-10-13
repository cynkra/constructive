#' Constructive options for class 'indMatrix'
#'
#' These options will be used on objects of class 'indMatrix' from the
#' 'Matrix' package, including objects of its subclass 'pMatrix'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as"` (default): We build the object using `as()` on the `perm` integer
#'   vector, or on a list of `perm` and the number of columns when it cannot be
#'   inferred, and apply `Matrix::t()` if the object's `margin` is 2.
#'   If this cannot reproduce the object exactly (e.g. dimnames) we fall back
#'   to the next constructor.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_indMatrix>
#' @export
opts_indMatrix <- function(constructor = c("as", "next"), ...) {
  .cstr_options("indMatrix", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct indMatrix
.cstr_construct.indMatrix <- function(x, ...) {
  opts <- list(...)$opts$indMatrix %||% opts_indMatrix()
  if (is_corrupted_indMatrix(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.indMatrix", structure(NA, class = opts$constructor))
}

is_corrupted_indMatrix <- function(x) {
  if (typeof(x) != "S4") return(TRUE)
  y <- do.call(methods::as, indMatrix_args(x))
  if (x@margin == 2L) y <- Matrix::t(y)
  !identical_slots(x, y)
}

#' @export
#' @method .cstr_construct.indMatrix as
.cstr_construct.indMatrix.as <- function(x, ...) {
  code <- .cstr_apply(indMatrix_args(x), "as", ...)
  if (x@margin == 2L) code <- .cstr_apply(list(code), "Matrix::t", ..., recurse = FALSE)
  repair_attributes_indMatrix(x, code, ...)
}

repair_attributes_indMatrix <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = methods::slotNames(x),
    idiomatic_class = class(x),
    flag_s4 = FALSE
  )
}

indMatrix_args <- function(x) {
  # number of columns of the (untransposed) index matrix
  n <- x@Dim[[3L - x@margin]]
  from <- if (n == max(x@perm, 0L)) x@perm else list(x@perm, n)
  cl <- class(x)
  attr(cl, "package") <- NULL
  list(from, cl)
}

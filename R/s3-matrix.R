#' Constructive options for matrices
#'
#' Matrices are atomic vectors, lists, or objects of type `"expression"` with a `"dim"`
#' attributes of length 2.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"matrix"` : We use `matrix()`
#' * `"array"` : We use `array()`
#' * `"cbind"`,`"rbind"` : We use `cbind()` or `"rbind()"`, this makes named
#'   columns and rows easier to read.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `"array"`
#' * `"atomic"` : We define as an atomic vector and repair attributes
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_matrix>
#' @export
opts_matrix  <- function(constructor = c("matrix", "array", "cbind", "rbind", "next"), ...) {
  .cstr_options("matrix", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct matrix
.cstr_construct.matrix <- function(x, ...) {
  opts <- list(...)$opts$matrix %||% opts_matrix()
  if (is_corrupted_matrix(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.matrix", structure(NA, class = opts$constructor))
}

is_corrupted_matrix <- function(x) {
  is_corrupted_array(x) || length(dim(x)) != 2
}

#' @export
#' @method .cstr_construct.matrix matrix
.cstr_construct.matrix.matrix <- function(x, ...) {
  dim <- attr(x, "dim")
  dimnames <- attr(x, "dimnames")
  dim_names_lst <- if (!is.null(dimnames)) list(dimnames = dimnames)
  x_stripped <- x
  attributes(x_stripped) <- NULL
  code <- .cstr_apply(
    c(list(x_stripped, nrow = dim[[1]], ncol = dim[[2]]), dim_names_lst),
    "matrix",
    ...
  )
  repair_attributes_matrix(x, code, ...)
}

#' @export
#' @method .cstr_construct.matrix array
.cstr_construct.matrix.array <- function(x, ...) {
  .cstr_construct.array.array(x, ...)
}

#' @export
#' @method .cstr_construct.matrix cbind
.cstr_construct.matrix.cbind <- function(x, ...) {
  dimnames <- attr(x, "dimnames")
  # apply(simplify = TRUE) needs R >= 4.1
  args <- lapply(
    as.data.frame(unclass(x)),
    set_names,
    dimnames[[1]]
  )
  names(args) <- dimnames[[2]]
  code <- .cstr_apply(args, "cbind", ...)
  repair_attributes_matrix(x, code, ...)
}

#' @export
#' @method .cstr_construct.matrix rbind
.cstr_construct.matrix.rbind <- function(x, ...) {
  dimnames <- attr(x, "dimnames")
  # apply(simplify = TRUE) needs R >= 4.1
  args <- lapply(
    as.data.frame(t(unclass(x))),
    set_names,
    dimnames[[2]]
  )
  names(args) <- dimnames[[1]]
  code <- .cstr_apply(args, "rbind", ...)
  repair_attributes_matrix(x, code, ...)
}

repair_attributes_matrix <- function(x, code, ..., pipe = NULL) {
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = c("dim", "dimnames")
  )
}

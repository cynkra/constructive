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
#' @param byrow Boolean. Only considered if `constructor` is `"matrix"`. If `TRUE`
#'   atomic matrices are constructed with `byrow = TRUE`, one row per line and
#'   with aligned columns, as with `tibble::tribble()`. Ignored for list and
#'   expression matrices, matrices with no row or column, and one liners.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_matrix>
#' @export
opts_matrix  <- function(constructor = c("matrix", "array", "cbind", "rbind", "next"), ..., byrow = FALSE) {
  abort_not_boolean(byrow)
  .cstr_options("matrix", constructor = constructor[[1]], ..., byrow = byrow)
}

#' @export
#' @method .cstr_construct matrix
.cstr_construct.matrix <- function(x, ...) {
  opts <- list(...)$opts$matrix %||% opts_matrix()
  if (is_corrupted_matrix(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.matrix", structure(NA, class = opts$constructor))
}

is_corrupted_matrix <- function(x) {
  dim <- attr(x, "dim")
  if (is.null(dim) || !is.integer(dim) || length(dim) != 2) return(TRUE)
  if (!(is.atomic(x) || is.list(x) || is.expression(x))) return(TRUE)
  FALSE
}

#' @export
#' @method .cstr_construct.matrix matrix
.cstr_construct.matrix.matrix <- function(x, ...) {
  opts <- list(...)$opts$matrix %||% opts_matrix()
  dim <- attr(x, "dim")
  if (isTRUE(opts$byrow) && is.atomic(x) && all(dim > 0) && !isTRUE(list(...)$one_liner)) {
    return(construct_matrix_byrow(x, ...))
  }
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

# Build the data with one row per line and aligned columns, like `tibble::tribble()`
construct_matrix_byrow <- function(x, ...) {
  opts <- list(...)$opts$matrix %||% opts_matrix()
  dim <- attr(x, "dim")
  dimnames <- attr(x, "dimnames")
  x_stripped <- x
  attributes(x_stripped) <- NULL
  cells <- vapply(
    x_stripped,
    function(cell) paste(.cstr_construct(cell, ...), collapse = ""),
    character(1)
  )
  # use NA rather than NA_character_ etc when relevant, as for atomic vectors
  typed_nas <- cells %in% c("NA_character_", "NA_real_", "NA_integer_", "NA_complex_")
  if (any(typed_nas) && !all(typed_nas)) cells[typed_nas] <- "NA"
  cells <- matrix(paste0(cells, ","), dim[[1]], dim[[2]])
  for (j in seq_len(dim[[2]])) cells[, j] <- format(cells[, j])
  rows <- apply(cells, 1, paste, collapse = " ")
  rows[[length(rows)]] <- sub(", *$", "", rows[[length(rows)]])
  rows <- sub(" +$", "", rows)
  args <- list(
    c("c(", indent(rows), ")"),
    nrow = .cstr_construct(dim[[1]], ...),
    ncol = .cstr_construct(dim[[2]], ...),
    byrow = "TRUE"
  )
  if (!is.null(dimnames)) args$dimnames <- .cstr_construct(dimnames, ...)
  code <- .cstr_apply(args, "matrix", ..., recurse = FALSE)
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

#' Constructive options for class 'ftable'
#'
#' These options will be used on objects of class 'ftable'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"ftable"` (default): We build the object using `ftable()` on a table,
#'   providing the `row.vars` argument when needed.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `opts_matrix("matrix")`.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_ftable>
#' @export
opts_ftable <- function(constructor = c("ftable", "next"), ...) {
  .cstr_options("ftable", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ftable
.cstr_construct.ftable <- function(x, ...) {
  opts <- list(...)$opts$ftable %||% opts_ftable()
  if (is_corrupted_ftable(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ftable", structure(NA, class = opts$constructor))
}

is_corrupted_ftable <- function(x) {
  row_vars <- attr(x, "row.vars")
  col_vars <- attr(x, "col.vars")
  if (!typeof(x) %in% c("integer", "double")) return(TRUE)
  if (!is.list(row_vars) || !is.list(col_vars)) return(TRUE)
  if (!length(row_vars) || !length(col_vars)) return(TRUE)
  vars <- c(row_vars, col_vars)
  if (!all(vapply(vars, is.character, logical(1))) || any(lengths(vars) == 0)) return(TRUE)
  expected_dim <- c(prod(lengths(row_vars)), prod(lengths(col_vars)))
  !identical(attr(x, "dim"), as.integer(expected_dim))
}

#' @export
#' @method .cstr_construct.ftable ftable
.cstr_construct.ftable.ftable <- function(x, ...) {
  row_vars <- attr(x, "row.vars")
  col_vars <- attr(x, "col.vars")
  # ftable() stores the data with the variables of each margin in reverse order,
  # this permutation is its own inverse
  n_row <- length(row_vars)
  perm <- c(rev(seq_len(n_row)), n_row + rev(seq_along(col_vars)))
  dimnames <- c(rev(row_vars), rev(col_vars))
  x_stripped <- x
  attributes(x_stripped) <- NULL
  tbl <- aperm(array(x_stripped, unname(lengths(dimnames)), dimnames), perm)
  class(tbl) <- "table"
  args <- list(tbl)
  # by default all variables but the last are row variables
  if (length(col_vars) > 1) {
    nms <- names(row_vars)
    use_nms <- !is.null(nms) && all(nzchar(nms)) && !anyDuplicated(names(dimnames))
    args$row.vars <- if (use_nms) nms else seq_len(n_row)
  }
  code <- .cstr_apply(args, "ftable", ...)
  repair_attributes_ftable(x, code, ...)
}

repair_attributes_ftable <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "ftable",
    ignore = c("dim", "row.vars", "col.vars")
  )
}

#' Constructive options for class 'table'
#'
#' These options will be used on objects of class 'table'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as.table"` (default): We build the object using `as.table()` on an array
#'   or a matrix. Objects of class "xtabs" are built this way too, their class
#'   and "call" attribute are then repaired.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `opts_matrix("matrix")` or `opts_array("array")`.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_table>
#' @export
opts_table <- function(constructor = c("as.table", "next"), ...) {
  .cstr_options("table", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct table
.cstr_construct.table <- function(x, ...) {
  opts <- list(...)$opts$table %||% opts_table()
  if (is_corrupted_table(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.table", structure(NA, class = opts$constructor))
}

is_corrupted_table <- function(x) {
  dim <- attr(x, "dim")
  dimnames <- attr(x, "dimnames")
  # as.table() fails on empty arrays and provides missing dimnames
  !typeof(x) %in% c("integer", "double") ||
    !is.integer(dim) ||
    !length(dim) ||
    any(dim == 0L) ||
    length(dimnames) != length(dim) ||
    any(vapply(dimnames, is.null, logical(1)))
}

#' @export
#' @method .cstr_construct.table as.table
.cstr_construct.table.as.table <- function(x, ...) {
  x_stripped <- x
  attributes(x_stripped) <- attributes(x)[c("dim", "dimnames")]
  code <- .cstr_apply(list(x_stripped), "as.table", ...)
  repair_attributes_table(x, code, ...)
}

repair_attributes_table <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "table",
    ignore = c("dim", "dimnames")
  )
}

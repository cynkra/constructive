#' @export
construct_idiomatic.array <- function(x, pipe, ...) {
  dim <- attr(x, "dim")
  dimnames <- attr(x, "dimnames")
  dim_names_lst <- if (!is.null(dimnames)) list(dimnames = dimnames)
  attr(x, "dim") <- NULL
  attr(x, "dimnames") <- NULL
  construct_apply(
    c(list(x, dim = dim), dim_names_lst),
    "array",
    new_line = FALSE
  )
}

#' @export
repair_attributes.array <- function(x, code, pipe ="base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    ignore = c("dim"),
    ...
  )
}

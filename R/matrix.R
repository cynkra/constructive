#' @export
construct_idiomatic.matrix <- function(x, ..., pipe = "base") {
  dim <- attr(x, "dim")
  dimnames <- attr(x, "dimnames")
  dim_names_lst <- if (!is.null(dimnames)) list(dimnames = dimnames)
  attr(x, "dim") <- NULL # for some reasons this remove dimnames too ???
  attr(x, "dimnames") <- NULL
  construct_apply(
    c(list(x, nrow = dim[[1]], ncol = dim[[2]]), dim_names_lst),
    ...,
    fun = "matrix",
    new_line = TRUE,
    pipe = pipe
  )
}

#' @export
repair_attributes.matrix <- function(x, code, ..., pipe ="base") {
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = c("dim", "dimnames")
  )
}

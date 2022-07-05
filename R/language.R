#' @export
construct_idiomatic.language <- function(x, ...) {
  wrap(rlang::expr_deparse(x), "quote", new_line = FALSE)
}

#' @export
repair_attributes.language <- function(x, code, pipe ="base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    ignore = c("srcref", "srcfile", "wholeSrcref"),
    ...
  )
}

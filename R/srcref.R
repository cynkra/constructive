#' @export
construct_idiomatic.srcref <- function(x, ...) {
  srcfile <- attr(x, "srcfile")
  attributes(x) <- NULL
  construct_apply(list(srcfile, x), "srcref", ...)
}

#' @export
repair_attributes.srcref<- function(x, code, ...) {
  repair_attributes_impl(
    x, code, ...,
    idiomatic_class = "srcref",
    ignore = "srcfile"
  )
}

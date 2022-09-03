#' @export
construct_idiomatic.language <- function(x, ...) {
  if (identical(x, quote(expr=))) return("quote(expr=)")
  attributes(x) <- NULL
  if (is_expression2(x)) {
    wrap(rlang::expr_deparse(x), "quote", new_line = FALSE)
  } else {
    list_call <- construct_apply(as.list(x), ...)
    wrap(list_call, "as.call", new_line = FALSE)
  }
}

#' @export
repair_attributes.language <- function(x, code, pipe ="base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    ignore = c("srcref", "srcfile", "wholeSrcref"),
    ...
  )
}

is_expression2 <- function(x) {
  non_srcref_attr_nms <-  setdiff(
    names(attributes(x)),
    c("srcref", "srcfile", "wholeSrcref")
  )
  if(length(non_srcref_attr_nms)) return(FALSE)
  if (rlang::is_syntactic_literal(x) || rlang::is_symbol(x)) return(TRUE)
  if(!rlang::is_call(x)) return(FALSE)
  all(vapply(x, is_expression2, logical(1)))
}

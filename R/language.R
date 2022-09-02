#' @export
construct_idiomatic.language <- function(x, ...) {
  if (identical(x, quote(expr=))) return("quote(expr=)")
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
  if (rlang::is_syntactic_literal(x) || rlang::is_symbol(x)) return(TRUE)
  if(!rlang::is_call(x)) return(FALSE)
  all(vapply(x, function(x) is.null(attributes(x)) && is_expression2(x), logical(1)))
}

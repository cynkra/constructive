#' Construct a function's signature
#'
#' @param x A function
#' @param name The name of the function, by default we use the symbol provided to `x`
#' @inheritParams deparse_call
#'
#' @export
#' @return A character vector with the class "vertical" (for nice printing with {styler})
#' @examples
#' construct_signature(lm)
construct_signature <- function(x, name = NULL, one_liner = FALSE, style = TRUE) {
  if (is.null(name)) {
    name <- as.character(substitute(x))
    if (length(name) > 1) abort("`name` should be of length 1")
  }
  fun_lst <- as.list(x)
  empty_lgl <- sapply(fun_lst, identical, quote(expr=))
  fun_lst[empty_lgl] <- lapply(names(fun_lst)[empty_lgl], as.symbol)
  names(fun_lst)[empty_lgl] <- ""
  names(fun_lst)[!empty_lgl] <- protect(names(fun_lst)[!empty_lgl])
  name <- protect(name)
  signature_lng <- as.call(c(as.symbol(name), fun_lst[-length(fun_lst)]))
  deparse_call(signature_lng, one_liner = one_liner, style = style)
}

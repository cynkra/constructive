#' @export
construct_idiomatic.function <- function(x, pipe, ...) {
  x_lst <- as.list(x)
  # rlang::expr_deparse changes the body by putting parentheses around f <- (function(){})
  # so we must use regular deparse
  fun_lst <- lapply(x_lst, deparse)
  args0 <- head(fun_lst, -1)
  body0 <- fun_lst[[length(fun_lst)]]
  # a srcref is created if the body starts with `{`
  srcrefed <- startsWith(body0[[1]], "{")
  args <- construct_apply(args0, "alist", language = TRUE, ...)
  body <- construct_apply(list(body0), "quote", language = TRUE, ...)
  env <- construct_raw(environment(x))
  code <- construct_apply(list(args, body, env), "rlang::new_function", language = TRUE)
  if (srcrefed) pipe(code, "rlang::zap_srcref()", pipe) else code
}

#' @export
repair_attributes.function <- function(x, code, pipe ="base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    ignore = c("name", "path"),
    ...
  )
}

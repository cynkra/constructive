#' @export
construct_idiomatic.function <- function(x, pipe, max_body = NULL, ...) {
  x_lst <- as.list(x)

  if (!is.null(max_body)) {
    body_lng <- x_lst[[length(x_lst)]]
    if (length(body_lng) > max_body + 1) {
      x_lst[[length(x_lst)]] <- as.call(c(head(as.list(body_lng), max_body + 1), quote(...)))
    }
  }
  # rlang::expr_deparse changes the body by putting parentheses around f <- (function(){})
  # so we must use regular deparse
  fun_lst <- lapply(x_lst, deparse)
  args0 <- head(fun_lst, -1)
  body0 <- fun_lst[[length(fun_lst)]]

  # a srcref is created if the body starts with `{`
  srcrefed <- startsWith(body0[[1]], "{")
  args <- construct_apply(args0, "alist", language = TRUE, pipe = pipe, max_body = max_body, ...)
  body <- construct_apply(list(body0), "quote", language = TRUE, pipe = pipe, max_body = max_body, ...)
  env <- construct_raw(environment(x), pipe = pipe, max_body = max_body, ...)
  code <- construct_apply(list(args, body, env), "rlang::new_function", language = TRUE, ...)
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

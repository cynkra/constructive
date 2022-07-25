#' @export
construct_idiomatic.function <- function(x, pipe, max_body = NULL, ...) {
  x_lst <- as.list(x)
  body_lng <- x_lst[[length(x_lst)]]
  if (!is.null(max_body)) {
    if (length(body_lng) > max_body + 1) {
      x_lst[[length(x_lst)]] <- as.call(c(head(as.list(body_lng), max_body + 1), quote(...)))
    }
  }
  # rlang::expr_deparse changes the body by putting parentheses around f <- (function(){})
  # so we must use regular deparse
  fun_lst <- lapply(x_lst, deparse)

  # as.function creates a srcref if the body starts with `{` so we remove it
  srcrefed <- identical(body_lng[[1]], as.symbol("{"))
  x_arg <- construct_apply(fun_lst, "alist", language = TRUE, pipe = pipe, max_body = max_body, ...)
  envir_arg <- construct_raw(environment(x), pipe = pipe, max_body = max_body, ...)
  code <- construct_apply(list(x_arg, envir = envir_arg), "as.function", language = TRUE, ...)
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

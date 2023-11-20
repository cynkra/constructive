#' @export
#' @rdname construct_reprex
setup_reprex <- function(fun, ...) {
  fun_lng <- substitute(fun)
  env <- environment(fun)
  fun_is_namespaced <-
    is.call(fun_lng) && (
      identical(fun_lng[[1]], quote(`::`)) ||
        identical(fun_lng[[1]], quote(`:::`))

    )
  if (fun_is_namespaced) {
    fun_chr <- as.character(fun_lng[[3]])
  } else {
    fun_chr <- as.character(fun_lng)
  }
  ns <- topenv(environment(fun))
  body_ <- as.list(body(fun))

  if (identical(body_[[1]], quote(`{`))) {
    body_ <- as.list(body_[-1])
  }

  fun_bkp <- fun
  tracing_code <- .cstr_apply(list(...), "constructive::construct_reprex")
  tracing_code[[1]] <- paste("constructive_reprex <- ", tracing_code[[1]])
  tracing_code <- parse(text = tracing_code)[[1]]
  body(fun) <- as.call(
    c(quote(`{`),
      tracing_code,
      quote(print(constructive_reprex)),
      body_
    ))
  attr(fun, "constructive_modified_from") <- fun_bkp
  # for cran checks
  a_i_n <- get("assignInNamespace")
  a_i_n(fun_chr, fun, ns)
  invisible(NULL)
}

#' @export
construct_idiomatic.function <- function(
    x, pipe, max_body = NULL, function.as.function = FALSE,
    function.zap_srcref = FALSE, function.construct_env = FALSE, one_liner = FALSE, ...) {
  x_lst <- as.list(x)
  body_lng <- x_lst[[length(x_lst)]]
  if (!is.null(max_body)) {
    if (length(body_lng) > max_body + 1) {
      x_lst[[length(x_lst)]] <- as.call(c(head(as.list(body_lng), max_body + 1), quote(...)))
    }
  }
  if (!function.as.function) {
    # FIXME: we should use the srcref
    code <- deparse(as.function(x_lst))
    if (length(code) == 2) code <- paste(code[1], code[2])
    if (function.construct_env || function.zap_srcref) {
      code[1] <- paste0("(", code[1])
      n <- length(code)
      code[n] <- paste0(code[n], ")")
    }
    if (function.construct_env) {
      envir_code <- construct_apply(
        list(environment(x)),
        'match.fun("environment<-")',
        function.as.function = function.as.function,
        function.zap_srcref = function.zap_srcref,
        function.construct_env = function.construct_env,
        one_liner = one_liner,
        ...)
      code <- pipe(code, envir_code, pipe, one_liner)
    }
  } else {
    # rlang::expr_deparse changes the body by putting parentheses around f <- (function(){})
    # so we must use regular deparse
    fun_lst <- lapply(x_lst, deparse)
    x_arg <- construct_apply(
      fun_lst, "alist", language = TRUE, pipe = pipe, max_body = max_body,
                             function.as.function = function.as.function, function.zap_srcref = function.zap_srcref,
                             function.construct_env = function.construct_env, one_liner = one_liner, ...)
    if (function.construct_env) {
      envir_arg <- construct_raw(environment(x), pipe = pipe, max_body = max_body, ...)
      code <- construct_apply(
        list(x_arg, envir = envir_arg), "as.function", language = TRUE,
        function.as.function = function.as.function, function.zap_srcref = function.zap_srcref,
        function.construct_env = function.construct_env, one_liner = one_liner, ...)
    } else {
      code <- construct_apply(
        list(x_arg), "as.function", language = TRUE,
        function.as.function = function.as.function, function.zap_srcref = function.zap_srcref,
        function.construct_env = function.construct_env, one_liner = one_liner, ...)
    }
  }

  if (function.zap_srcref) {
    # a srcref is created if the body starts with `{` so we remove it
    srcrefed <- is.call(body_lng) && identical(body_lng[[1]], as.symbol("{"))
    if (srcrefed) code <- pipe(code, "rlang::zap_srcref()", pipe, one_liner)
  }
  code
}

#' @export
repair_attributes.function <- function(x, code, pipe ="base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    ignore = c("name", "path"),
    ...
  )
}

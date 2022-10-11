#' Constructive options for functions
#'
#' These options will be used on functions, i.e. objects of type "closure", "special" and "builtin".
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"function"` (default): Build the object using a standard `function() {}`
#'   definition. This won't set the environment by default, unless `environment`
#'   is set to `TRUE`
#' * `"as.function"` : Build the object using a `as.function()` call, by default will
#'   attempt to recreate the environment.
#'   back to `data.frame()`.
#' * `"new_function"` : Build the object using a `rlang::new_function()` call, by default will
#'   attempt to recreate the environment.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @param zap_srcref Boolean. Whether to pipe the output to `rlang::zapo_srcref()` which will
#'   get rid recursively of all srcref related attributes.
#' @param environment Boolean. Whether to attempt to reconstruct the function's environment,
#'   `FALSE` by default with the default `"function"` constructor. `TRUE` by default otherwise.
#' @param trim `NULL` or integerish. Maximum of lines showed in the body before it's trimmed,
#' replacing code with `...`. Note that it will necessarily produce code that doesn't
#' reproduce the input, this will parse and evaluate without failure.
#'
#' @return An object of class <constructive_options/constructive_options_function>
#' @export
opts_function <- function(
    constructor = c("function", "as.function", "new_function"),
    ...,
    zap_srcref = FALSE,
    environment = constructor != "function",
    trim = NULL) {
  combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty(),
    abort_not_boolean(zap_srcref),
    abort_not_boolean(environment),
    abort_not_null_or_integerish(trim)
  )
  structure(
    class = c("constructive_options", "constructive_options_function"),
    list(constructor = constructor, zap_srcref = zap_srcref, environment = environment, trim = trim)
  )
}


#' @export
construct_idiomatic.function <- function(
    x, ..., pipe, one_liner = FALSE) {
  if (rlang::is_primitive(x)) return(deparse(`+`))
  opts <- fetch_opts("function", ...)
  trim <- opts$trim
  constructor <- opts$constructor
  environment <- opts$environment

  x_lst <- as.list(x)
  body_lng <- x_lst[[length(x_lst)]]
  zap_srcref <-  opts$zap_srcref && is.call(body_lng) && identical(body_lng[[1]], as.symbol("{"))

  # trim if relevant
  if (!is.null(trim)) {
    if (length(body_lng) > trim + 1) {
      x_lst[[length(x_lst)]] <- as.call(c(head(as.list(body_lng), trim + 1), quote(...)))
    }
  }

  if (constructor == "function") {
    # FIXME: we should use the srcref
    fun_call <- call("function")
    fun_call[[2]] <- as.pairlist(x_lst[-length(x_lst)])
    fun_call[[3]] <- x_lst[[length(x_lst)]]
    code <- deparse_call(fun_call, pipe = FALSE, one_liner = one_liner, style = FALSE)

    if (length(code) == 2) code <- paste(code[1], code[2])
    if (environment || zap_srcref) {
      code[1] <- paste0("(", code[1])
      n <- length(code)
      code[n] <- paste0(code[n], ")")
    }
    if (environment) {
      envir_code <- construct_apply(
        list(environment(x)),
        'match.fun("environment<-")',
        pipe = pipe,
        one_liner = one_liner,
        ...)
      code <- pipe(code, envir_code, pipe, one_liner)
    }
  } else if (constructor == "as.function") {
    # rlang::expr_deparse changes the body by putting parentheses around f <- (function(){})
    # so we must use regular deparse
    fun_lst <- lapply(x_lst, deparse)
    args <- list(construct_apply(
      fun_lst, "alist", ..., language = TRUE, pipe = pipe, one_liner = one_liner))
    if (environment) {
      envir_arg <- construct_raw(environment(x), ..., pipe = pipe, one_liner = one_liner)
      args <- c(args, list(envir = envir_arg))
    }
    code <- construct_apply(args, "as.function", ..., language = TRUE, pipe = pipe, one_liner = one_liner)
  } else if (constructor == "new_function") {
    # rlang::expr_deparse changes the body by putting parentheses around f <- (function(){})
    # so we must use regular deparse
    fun_lst <- lapply(x_lst, deparse)
    args <- list(
      args = construct_apply(fun_lst[-length(fun_lst)], "alist", ..., language = TRUE, pipe = pipe, one_liner = one_liner),
      body = wrap(fun_lst[[length(fun_lst)]], "quote", new_line = FALSE)
    )
    if (environment) {
      envir_arg <- construct_raw(environment(x), ..., pipe = pipe, one_liner = one_liner)
      args <- c(args, list(env = envir_arg))
    }
    code <- construct_apply(args, "rlang::new_function", ..., language = TRUE, pipe = pipe, one_liner = one_liner)
  }

  # zap srcref if relevant
  if (zap_srcref) {
    code <- pipe(code, "rlang::zap_srcref()", pipe, one_liner)
  }
  code
}

#' @export
repair_attributes.function <- function(x, code, ..., pipe ="base") {
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = c("name", "path")
  )
}

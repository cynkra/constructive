constructors$`function` <- new.env()

#' Constructive options for functions
#'
#' These options will be used on functions, i.e. objects of type "closure", "special" and "builtin".
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"function"` (default): Build the object using a standard `function() {}`
#'   definition. This won't set the environment by default, unless `environment`
#'   is set to `TRUE`. If a srcref is available, if this srcref matches the function's
#'   definition, and if `trim` is left `NULL`, the code is returned from using the srcref,
#'   so comments will be shown in the output of `construct()`.
#' * `"as.function"` : Build the object using a `as.function()` call.
#'   back to `data.frame()`.
#' * `"new_function"` : Build the object using a `rlang::new_function()` call.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @param environment Boolean. Whether to reconstruct the function's environment.
#' @param srcref Boolean. Whether to attempt to reconstruct the function's srcref.
#' @param trim `NULL` or integerish. Maximum of lines showed in the body before it's trimmed,
#' replacing code with `...`. Note that it will necessarily produce code that doesn't
#' reproduce the input, but it will parse and evaluate without failure.
#'
#' @return An object of class <constructive_options/constructive_options_function>
#' @export
opts_function <- function(
    constructor = c("function", "as.function", "new_function"),
    ...,
    environment = TRUE,
    srcref = FALSE,
    trim = NULL) {
  combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty(),
    abort_not_boolean(environment),
    abort_not_boolean(srcref),
    abort_not_null_or_integerish(trim)
  )
  constructive_options("function", constructor = constructor, environment = environment, srcref = srcref, trim = trim)
}


#' @export
construct_raw.function <- function(
    x, ..., pipe, one_liner = FALSE) {
  if (rlang::is_primitive(x)) return(deparse(x))
  opts <- fetch_opts("function", ...)
  if (is_corrupted_function(x)) return(NextMethod())

  # trim if relevant
  trim <- opts$trim
  if (!is.null(trim)) {
    x_lst <- as.list(unclass(x))
    x_length <- length(x_lst)
    body_lng <- x_lst[[x_length]]
    if (length(body_lng) > trim + 1) {
      x_lst[[x_length]] <- as.call(c(head(as.list(body_lng), trim + 1), quote(...)))
      x <- as.function(x_lst, envir = environment(x))
    }
  }

  constructor <- constructors$`function`[[opts$constructor]]
  constructor(x, ..., trim = opts$trim, environment = opts$environment, srcref = opts$srcref)
}

#' @export
is_corrupted_function<- function(x) {
  !is.function(x)
}

constructors$`function`$`function` <- function(x, ..., pipe = "base", one_liner = FALSE, trim, environment, srcref) {
  # if the srcref matches the function's body (always in non artifical cases)
  # we might use the srcref rather than the body, so we keep the comments

  code_from_srcref <- FALSE
  if (!one_liner && is.null(trim)) {
    code <- code_from_srcref(x)
    if(!is.null(code)) {
      code_from_srcref <- TRUE
    }
  }

  if (!code_from_srcref) {
    fun_call <- call("function")
    x_lst <- as.list(unclass(x))
    x_length <- length(x_lst)
    if (x_length > 1) {
      fun_call[[2]] <- as.pairlist(x_lst[-x_length])
    }
    fun_call[3] <- x_lst[x_length]
    code <- deparse_call(fun_call, pipe = FALSE, one_liner = one_liner, style = FALSE)

    if (length(code) == 2) code <- paste(code[1], code[2])
  }

  attrs <- attributes(x)
  if (!srcref) attrs$srcref <- NULL

  if (environment || length(attrs)) {
    code <- wrap(code, fun = "")
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
  repair_attributes.function(x, code, ..., pipe = pipe, one_liner = one_liner)
}

constructors$`function`$as.function <- function(x, ..., trim, environment, srcref) {
  # rlang::expr_deparse changes the body by putting parentheses around f <- (function(){})
  # so we must use regular deparse

  x_lst <- as.list(unclass(x))
  fun_lst <- lapply(x_lst, deparse)
  args <- list(construct_apply(
    fun_lst, "alist", ..., language = TRUE))
  if (environment) {
    envir_arg <- construct_raw(environment(x), ...)
    args <- c(args, list(envir = envir_arg))
  }
  code <- construct_apply(args, "as.function", ..., language = TRUE)
  repair_attributes.function(x, code, ...)
}

constructors$`function`$new_function <- function(x, ..., trim, environment, srcref) {
  # rlang::expr_deparse changes the body by putting parentheses around f <- (function(){})
  # so we must use regular deparse

  x_lst <- as.list(unclass(x))
  fun_lst <- lapply(x_lst, deparse)
  args <- list(
    args = construct_apply(fun_lst[-length(fun_lst)], "alist", ..., language = TRUE),
    body = wrap(fun_lst[[length(fun_lst)]], "quote", new_line = FALSE)
  )
  if (environment) {
    envir_arg <- construct_raw(environment(x), ...)
    args <- c(args, list(env = envir_arg))
  }
  code <- construct_apply(args, "rlang::new_function", ..., language = TRUE)
  repair_attributes.function(x, code, ...)
}

#' @export
repair_attributes.function <- function(x, code, ..., pipe ="base") {
  opts <- fetch_opts("function", ...)
  srcref <- opts$srcref
  ignore <- c("name", "path")
  if (!srcref) ignore <- c(ignore, "srcref")
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = ignore
  )
}

# returns the srcref as a character vector IF it matches the actual function, NULL otherwise
code_from_srcref <- function(x) {
  srcref <- attr(x, "srcref")
  # srcref might have been zapped or function built withoiut srcref
  if (is.null(srcref)) return(NULL)
  srcref_chr <- as.character(srcref)
  # srcref might have been manipulated and not parseable -> try
  parsed <- try(parse(text=srcref_chr)[[1]], silent = TRUE)
  if (
    inherits(parsed, "try-error") ||
    # don't bother trying to eval if it's not a function call
    !identical(parsed[[1]], quote(`function`)) ||
    # Note : ignore.srcef = TRUE is not enough, it might just look at the srcref attribute at the top level
    !identical(rlang::zap_srcref(eval(parsed)), rlang::zap_srcref(x), ignore.environment = TRUE)
  ) {
    return(NULL)
  }
  srcref_chr
}

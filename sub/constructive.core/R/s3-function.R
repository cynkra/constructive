#' Constructive options for functions
#'
#' These options will be used on functions, i.e. objects of type "closure", "special" and "builtin".
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"function"` (default): Build the object using a standard `function() {}`
#'   definition. This won't set the environment by default, unless `environment`
#'   is set to `TRUE`. If a srcref is available, if this srcref matches the function's
#'   definition, and if `trim` is left `NULL`, the code is returned from using the srcref,
#'   so comments will be shown in the output of `construct()`. In the rare case
#'   where the ast body of the function contains non syntactic nodes this constructor
#'   cannot be used and falls back to the `"as.function"` constructor.
#' * `"as.function"` : Build the object using a `as.function()` call.
#'   back to `data.frame()`.
#' * `"new_function"` : Build the object using a `rlang::new_function()` call.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
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
  .cstr_combine_errors(
    check_dots_empty(),
    abort_not_boolean(environment),
    abort_not_boolean(srcref),
    abort_not_null_or_integerish(trim)
  )
  .cstr_options("function", constructor = constructor[[1]], environment = environment, srcref = srcref, trim = trim)
}

#' @export
#' @method .cstr_construct function
.cstr_construct.function <- function(x, ...) {
  if (rlang::is_primitive(x)) return(deparse(x))
  opts <- list(...)$opts$`function` %||% opts_function()
  if (is_corrupted_function(x)) return(NextMethod())
  UseMethod(".cstr_construct.function", structure(NA, class = opts$constructor))
}

is_corrupted_function <- function(x) {
  !is.function(x)
}

#' @export
#' @method .cstr_construct.function function
.cstr_construct.function.function <- function(x, ...) {
  opts <- list(...)$opts$`function` %||% opts_function()
  trim <- opts$trim
  environment <- opts$environment && !identical(environment(x), list(...)$env)
  srcref <- opts$srcref

  x_bkp <- x
  if (!is.null(trim)) x <- trim_function(x, trim)

  # if the srcref matches the function's body (always in non artifical cases)
  # we might use the srcref rather than the body, so we keep the comments

  x_lst <- as.list(unclass(x))
  x_length <- length(x_lst)

  all_components_are_proper_expressions <-
    all(vapply(x_lst, is_expression2, logical(1)))

  if (!all_components_are_proper_expressions) {
    # fall back on `as.function()` constructor
    res <- .cstr_construct.function.as.function(x, ...)
    return(res)
  }

  code_from_srcref <- FALSE
  if (!list(...)$one_liner && is.null(trim)) {
    code <- code_from_srcref(x)
    if (!is.null(code)) {
      code_from_srcref <- TRUE
    }
  }

  if (!code_from_srcref) {
    fun_call <- call("function")
    if (x_length > 1) {
      fun_call[[2]] <- as.pairlist(x_lst[-x_length])
    }
    fun_call[3] <- x_lst[x_length]
    code <- deparse_call0(fun_call, ...)
    if (length(code) == 2) code <- paste(code[1], code[2])
  }

  attrs <- attributes(x)
  if (!srcref) attrs$srcref <- NULL

  remove_srcref <- srcref && is.null(attr(x, "srcref"))

  if (environment || length(attrs) || remove_srcref) {
    code <- .cstr_wrap(code, fun = "")
  }
  if (environment) {
    envir_code <- .cstr_apply(list(environment(x)), "(`environment<-`)", ...)
    code <- .cstr_pipe(code, envir_code, ...)
  }
  repair_attributes_function(x_bkp, code, remove_srcref = remove_srcref, ...)
}

#' @export
#' @method .cstr_construct.function as.function
.cstr_construct.function.as.function <- function(x, ...) {
  opts <- list(...)$opts$`function` %||% opts_function()
  trim <- opts$trim
  environment <- opts$environment
  srcref <- opts$srcref

  x_bkp <- x
  if (!is.null(trim)) x <- trim_function(x, trim)

  x_lst <- as.list(unclass(x))

  all_components_are_proper_expressions <-
    all(vapply(x_lst, is_expression2, logical(1)))

  if (all_components_are_proper_expressions) {
    fun_lst <- lapply(x_lst, deparse_call0, ...)
    args <- list(.cstr_apply(fun_lst, "alist", ..., recurse = FALSE))
  } else {
    fun_lst <- lapply(x_lst, function(x, ...) .cstr_construct(x, ...), ...)
    args <- list(.cstr_apply(fun_lst, "list", ..., recurse = FALSE))
  }

  if (environment) {
    envir_arg <- .cstr_construct(environment(x), ...)
    args <- c(args, list(envir = envir_arg))
  }
  code <- .cstr_apply(args, "as.function", ..., recurse = FALSE)
  repair_attributes_function(x_bkp, code, ...)
}

#' @export
#' @method .cstr_construct.function new_function
.cstr_construct.function.new_function <- function(x, ...) {
  opts <- list(...)$opts$`function` %||% opts_function()
  trim <- opts$trim
  environment <- opts$environment
  srcref <- opts$srcref

  x_bkp <- x
  if (!is.null(trim)) x <- trim_function(x)

  x_lst <- as.list(unclass(x))

  args <- lapply(x_lst[-length(x_lst)], deparse_call0, ...)
  args <- .cstr_apply(args, "alist", ..., recurse = FALSE)
  body <- .cstr_construct.language(x_lst[[length(x_lst)]], ...)

  args <- list(args = args, body = body)
  if (environment) {
    envir_arg <- .cstr_construct(environment(x), ...)
    args <- c(args, list(env = envir_arg))
  }
  code <- .cstr_apply(args, "rlang::new_function", ..., recurse = FALSE)
  repair_attributes_function(x_bkp, code, ...)
}

repair_attributes_function <- function(x, code, remove_srcref = FALSE, ...) {
  opts <- list(...)$opts$`function` %||% opts_function()
  srcref <- opts[["srcref"]]
  ignore <- if (!srcref) "srcref"
  remove <- if (remove_srcref) "srcref"
  .cstr_repair_attributes(x, code, ..., remove = remove, ignore = ignore)
}

# returns the srcref as a character vector IF it matches the actual function, NULL otherwise
code_from_srcref <- function(x) {
  srcref <- attr(x, "srcref")
  # srcref might have been zapped or function built withoiut srcref
  if (is.null(srcref)) return(NULL)
  srcref_chr <- as.character(srcref)
  # srcref might have been manipulated and not parseable -> try
  parsed <- try(parse(text = srcref_chr)[[1]], silent = TRUE)
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

trim_function <- function(x, trim) {
  x_lst <- as.list(unclass(x))
  x_length <- length(x_lst)
  body_lng <- x_lst[[x_length]]
  if (length(body_lng) > trim + 1) {
    x_lst[[x_length]] <- as.call(c(head(as.list(body_lng), trim + 1), quote(...)))
    x <- as.function(x_lst, envir = environment(x))
  }
  x
}

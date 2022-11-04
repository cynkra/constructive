#' Build code to recreate an object
#'
#' @param x An object
#'
#' @param data named list of objects we don't want to deparse, can also be a package
#' name and its namespace and datasets will be used to look for objects. Both can
#' be combined so you can provide a list of named objects and unnamed namespaces.
#'
#' @param pipe Which pipe to use, either "base" or "magrittr"
#' @param check Boolean. Whether to check if the created code reproduces the object
#'   using `waldo::compare()`
#' @param max_atomic maximum number of elements of atomic vectors to print, forces check to `FALSE`
#' @param max_list maximum number of elements of a list to print, forces check to `FALSE`
#' @param max_body maximum number of calls to show from a function's body, forces check to `FALSE`
#' @param env_as_list translate environments to `new.env()` rather than `as.environment(list(...))`
#' @param ignore_srcref,ignore_attr,ignore_function_env,ignore_formula_env passed to `waldo::compare()`
#' @param ... Additional parameters passed to `construct_impl()` generic and methods.
#'
#' @export
construct <- function(x, data = NULL, pipe = c("base", "magrittr"), check = NULL,
                      max_atomic = NULL, max_list = NULL, max_body = NULL, env_as_list = TRUE,
                      ignore_srcref = TRUE, ignore_attr = FALSE, ignore_function_env = FALSE, ignore_formula_env = FALSE, one_liner = FALSE, ...) {
  pipe <- match.arg(pipe)
  data <- preprocess_data(data)
  code <- try_construct(x, data, pipe = pipe, max_atomic = max_atomic, max_body = max_body, max_list = max_list, env_as_list = env_as_list, one_liner = one_liner, ...)
  styled_code <- try_parse(code, data, one_liner)
  if (!is.null(max_atomic) || !is.null(max_list) || !is.null(max_body)) {
    check <- FALSE
  }
  compare <- check_round_trip(x, styled_code, data, check, ignore_srcref, ignore_attr, ignore_function_env, ignore_formula_env)
  structure(list(code = styled_code, compare = compare), class = "constructive")
}

# helpers for the above --------------------------------------------------------

globals <- new.env()

#' Show constructive issues
#'
#' @param x An object built by `construct()`, if `NULL` the latest encountered
#'   issues will be displayed
#'
#' @return A character vector with class "waldo_compare"
#' @export
construct_issues <- function(x = NULL) {
  if (is.null(x)) return(globals$issues)
  x$compare
}

#' @export
print.constructive <- function(x) {
  print(x$code)
  invisible(x)
}

preprocess_data <- function(data) {
  if (is.character(data)) return(namespace_as_list(data))
  if (is.environment(data)) return(as.list(data))
  # recurse into unnamed elements
  nms <- rlang::names2(data)
  named_elts <-  data[nms != ""]
  unnamed_elts <-  data[nms == ""]
  c(named_elts, do.call(c, lapply(unnamed_elts, preprocess_data)))
}

try_construct <- function(...) {
  caller <- caller_env()
  rlang::try_fetch(construct_raw(...), error = function(e) {
    #nocov start
    abort("{constructive} could not build the requested code.", parent = e, call = caller)
    #nocov end
  })
}

try_parse <- function(code, data, one_liner) {
  caller <- caller_env()
  scope <- if (one_liner) "indention" else "line_breaks"
  rlang::try_fetch(
    styler::style_text(code, scope = scope),
    error = function(e) {
      #nocov start
      abort("The code built by {constructive} could not be parsed.", parent = e, call = caller)
      #nocov end
    }
  )
}

try_eval <- function(styled_code, data) {
  caller <- caller_env()
  rlang::try_fetch(
    eval(parse(text = styled_code), envir = data, enclos = caller),
    error = function(e) {
      #nocov start
      print(styled_code)
      abort("The code built by {constructive} could not be evaluated.", parent = e, call = caller)
      #nocov end
    }
  )
}

check_round_trip <- function(x, styled_code, data, check, ignore_srcref, ignore_attr, ignore_function_env, ignore_formula_env) {
  if (isFALSE(check)) return(NULL)
  evaled <- try_eval(styled_code, data)
  if (missing(evaled)) return(NULL)
  out <- waldo::compare(
    x,
    evaled,
    x_arg = "original",
    y_arg = "recreated",
    ignore_srcref = ignore_srcref,
    ignore_attr = ignore_attr,
    ignore_encoding = TRUE,
    ignore_function_env = ignore_function_env,
    ignore_formula_env = ignore_formula_env
  )
  if (!length(out)) return(NULL)

  globals$issues <- out
  msg <- "{constructive} couldn't create code that reproduces perfectly the input"
  if (isTRUE(check)) {
    print(styled_code)
    msg <- paste0(msg, "\n", paste(out, collapse = "\n"))
    abort(c(msg))
  }
  info <- "Call `construct_issues()` to inspect the last issues"
  rlang::inform(c(msg, i = info))
  out
}



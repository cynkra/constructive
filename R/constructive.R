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
#' @param ignore_srcref,ignore_attr,ignore_function_env,ignore_formula_env passed to `waldo::compare()`
#' @param ... Constructive options built with the `opts_*()` family of functions. See the "Constructive options"
#'   section below.
#' @param one_liner Boolean. Whether to collapse the output to a single line of code.
#' @param template A list of constructive options build with `opts_*()` functions,
#'   they will be overriden by `...`. This is designed to help users set a default
#'   behavior for `{constructive}`.
#'
#' @section Constructive options:
#'
#' Constructive options provide a way to customize the output of `construct()`.
#' We can provide calls to `opts_*()` functions to the `...` argument. Each of
#' these functions targets a specific element type and is documented on its own page.
#'
#' * [opts_atomic()]
#' * [opts_data.frame()]
#' * [opts_Date()]
#' * [opts_environment()]
#' * [opts_formula()]
#' * [opts_factor()]
#' * [opts_function()]
#' * [opts_list()]
#' * [opts_ordered()]
#' * [opts_POSIXct()]
#' * [opts_tbl_df()]
#'
#' In particular by default the environments of functions and formulas are not reconstructed,
#' and `opts_formula()` and `opts_function()` help you adjust this behavior.
#' Note that objects referring to environment often can't be reconstructed faithfully.
#' Some compromises have to be made and `opts_environment()` helps you make them.
#' Other `opts_*()` functions have a purely cosmetic effect.
#'
#' @export
construct <- function(x, ..., data = NULL, pipe = c("base", "magrittr"), check = NULL,
                      ignore_srcref = TRUE, ignore_attr = FALSE, ignore_function_env = FALSE, ignore_formula_env = FALSE, one_liner = FALSE,
                      template = getOption("constructive_opts_template")) {
  combine_errors(
    ellipsis::check_dots_unnamed(),
    # FIXME: check data
    pipe <- rlang::arg_match(pipe),
    abort_not_boolean(ignore_srcref),
    abort_not_boolean(ignore_attr),
    abort_not_boolean(ignore_function_env),
    abort_not_boolean(ignore_formula_env),
    abort_not_boolean(one_liner)
    # FIXME: check template
  )
  data <- preprocess_data(data)
  code <- try_construct(x, template = template, ..., data = data, pipe = pipe, one_liner = one_liner)
  styled_code <- try_parse(code, data, one_liner)
  caller <- caller_env()
  compare <- check_round_trip(x, styled_code, data, check, ignore_srcref, ignore_attr, ignore_function_env, ignore_formula_env, caller)
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
print.constructive <- function(x, ...) {
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

try_construct <- function(x, ...) {
  caller <- caller_env()
  rlang::try_fetch(construct_raw(x, ...), error = function(e) {
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

try_eval <- function(styled_code, data, check, caller) {
  rlang::try_fetch(
    eval(parse(text = styled_code), envir = data, enclos = caller),
    error = function(e) {
      #nocov start
      msg <- "The code built by {constructive} could not be evaluated."
      if (isTRUE(check)) {
        print(styled_code)
        abort(msg, parent = e, call = caller)
      }
      rlang::inform(c("!" = msg))
      #nocov end
    }
  )
}

check_round_trip <- function(x, styled_code, data, check, ignore_srcref, ignore_attr, ignore_function_env, ignore_formula_env, caller) {
  if (isFALSE(check)) return(NULL)
  evaled <- try_eval(styled_code, data, check, caller)
  if (missing(evaled) || (is.null(evaled) && !is.null(x))) return(NULL)
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



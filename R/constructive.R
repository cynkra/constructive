#' Build code to recreate an object
#'
#' @param x An object
#'
#' @param data named list of data we don't want to deparse
#' @param check Boolean. Whether to check if the created code reproduces the object
#'   exactly (using `identical()`)
#' @param pipe Which pipe to use, either "base" or "magrittr"
#' @param check should we try to eval the result and check if it is identical to
#'   `data`
#' @param max_atomic maximum number of elements of atomic vectors to print, forces check to `FALSE`
#' @param max_list maximum number of elements of a list to print, forces check to `FALSE`
#' @param max_body maximum number of calls to show from a function's body, forces check to `FALSE`
#' @param env_as_list translate environments to `new.env()` rather than `as.environment(list(...))`
#' @param ignore_srcref whether to ignore all srcref attributes in the check
#' @param ... Additional parameters passed to `construct_impl()` generic and methods.
#'
#' @export
construct <- function(x, data = NULL, pipe = c("base", "magrittr"), check = TRUE, max_atomic = NULL, max_list = NULL, max_body = NULL, env_as_list = TRUE, ignore_srcref = TRUE, ...) {
  pipe <- match.arg(pipe)
  data <- preprocess_data(data)
  code <- try_construct(x, data, pipe = pipe, max_atomic = max_atomic, max_body = max_body, max_list = max_list, env_as_list = env_as_list, ...)
  styled_code <- try_parse(code, data)
  check <- check && is.null(max_atomic) && is.null(max_list) && is.null(max_body)
  if (check) {
    evaled <- try_eval(styled_code, data)
    check_round_trip(x, evaled, styled_code, ignore_srcref = ignore_srcref)
  }
  styled_code
}

# helpers for the above --------------------------------------------------------

preprocess_data <- function(data) {
  if (is.character(data)) data <- namespace_as_list(data)
  if (is.environment(data)) data <- as.list(data)
  data
}

try_construct <- function(...) {
  caller <- caller_env()
  rlang::try_fetch(construct_raw(...), error = function(e) {
    #nocov start
    abort("{constructive} could not build the requested code.", parent = e, call = caller)
    #nocov end
  })
}

try_parse <- function(code, data) {
  caller <- caller_env()
  rlang::try_fetch(
    styler::style_text(code, scope = "line_breaks"),
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

# FIXME: we might not be identical and still have waldo not find any difference
#   we should print something in those cases.
# FIXME: we should be able to set the ignore_* args from `construct()`, `identical()`
#   itself has args `ignore.bytecode`, `ignore.environment` and `ignore.srcref`
#   that we can use. We might sometimes have to do the comparison using `waldo()` directly though.
check_round_trip <- function(x, evaled, styled_code, ignore_srcref) {
  caller <- caller_env()
  if (ignore_srcref) {
    if(identical(x, quote(expr=)) && missing(evaled)) return(invisible(NULL))
    x <- rlang::zap_srcref(x)
    evaled <- rlang::zap_srcref(evaled)
  }
  if (!identical(x, evaled)) {
    #nocov start
    print(styled_code)
    comparison <- waldo::compare(
      x,
      evaled,
      x_arg = "original",
      y_arg = "recreated",
      ignore_srcref = FALSE,
      ignore_attr = FALSE,
      ignore_encoding = FALSE,
      ignore_function_env = FALSE,
      ignore_formula_env = FALSE
    )
    abort(
      c(
        paste0(
          "{constructive} couldn't create code that reproduces perfectly the output\n",
          paste(comparison, collapse = "\n")
        ),
        i = "use `check = FALSE` to ignore this error"
      ),
      call = caller
    )
    #nocov end
  }
  invisible(NULL)
}



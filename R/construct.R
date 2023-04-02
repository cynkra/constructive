#' Build code to recreate an object
#'
#' `construct()` builds the code to reproduce one object, `construct_multi()`
#' builds the code to reproduce objects stored in a named list or environment.
#'
#' @param x An object, for `construct_multi()` a named list or an environment.
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
#' @return An object of class 'constructive'
#' @enumerateOptFunctions
#'
#' @export
construct <- function(x, ..., data = NULL, pipe = c("base", "magrittr"), check = NULL,
                      ignore_srcref = TRUE, ignore_attr = FALSE, ignore_function_env = FALSE, ignore_formula_env = FALSE, one_liner = FALSE,
                      template = getOption("constructive_opts_template")) {
  # force so we might fail outside of the try_fetch() when x is not properly provided
  force(x)
  # reset globals
  globals$predefinition <- character()
  globals$envs <- data.frame(hash = character(), name = character())

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
  code <- c(globals$predefinition, code)
  Encoding(code) <- "UTF-8"
  styled_code <- try_parse(code, data, one_liner)
  caller <- caller_env()
  compare <- check_round_trip(x, styled_code, data, check, ignore_srcref, ignore_attr, ignore_function_env, ignore_formula_env, caller)
  new_constructive(styled_code, compare)
}

#' @export
#' @rdname construct
construct_multi <- function(x, ..., data = NULL, pipe = c("base", "magrittr"), check = NULL,
                            ignore_srcref = TRUE, ignore_attr = FALSE, ignore_function_env = FALSE, ignore_formula_env = FALSE, one_liner = FALSE,
                            template = getOption("constructive_opts_template")) {
  abort_not_env_or_named_list(x)
  if (is.environment(x)) x <- as.list.environment(x)
  data <- preprocess_data(data)
  constructives <- lapply(
    x, construct,  ...,
    data = data, pipe = pipe, check = check,
    ignore_srcref = ignore_srcref, ignore_attr = ignore_attr,
    ignore_function_env = ignore_function_env,
    ignore_formula_env = ignore_formula_env, one_liner = one_liner,
    template = template
  )
  code <- lapply(constructives, `[[`, "code")
  issues <- lapply(constructives, `[[`, "compare")
  issues <- Filter(Negate(is.null), issues)
  globals$issues <- issues
  code <-  Map(
    code, names(code),
    f = function(x, y) {
      x[[1]] <- paste(protect(y), "<-", x[[1]])
      c(x, "")
    })
  code <- unlist(code)
  Encoding(code) <- "UTF-8"
  if (is.null(code)) code <- character(0)
  class(code) <- "vertical"
  new_constructive(unname(code), compare)
}

#' @export
print.constructive <- function(x, ...) {
  print(x$code)
  invisible(x)
}

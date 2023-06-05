# Functions that are called in construct, or functions called only by the former

#' Options for waldo::compare
#'
#' Builds options that will be passed to `waldo::compare()` down the line.
#'
#' @inheritParams waldo::compare
#'
#' @return A list
#' @export
compare_options <- function(ignore_srcref = TRUE, ignore_attr = FALSE, ignore_function_env = FALSE, ignore_formula_env = FALSE) {
  .cstr_combine_errors(
    abort_not_boolean(ignore_srcref),
    abort_not_boolean(ignore_attr),
    abort_not_boolean(ignore_function_env),
    abort_not_boolean(ignore_formula_env)
  )
  structure(
    list(
      ignore_srcref = ignore_srcref,
      ignore_attr = ignore_attr,
      ignore_function_env = ignore_function_env,
      ignore_formula_env = ignore_formula_env
    ),
    class = "constructive_compare_options"
  )
}

process_data <- function(data, main = TRUE) {
  if (is.character(data) && length(data) == 1) return(namespace_as_list(data, main = main))
  if (is.environment(data)) return(as.list(data))
  # recurse into unnamed elements
  nms <- rlang::names2(data)
  named_elts <-  data[nms != ""]
  unnamed_elts <-  data[nms == ""]
  objs <- c(named_elts, do.call(c, lapply(unnamed_elts, process_data, main = FALSE)))
  if (main) {
    if (anyDuplicated(names(objs))) {
      dupes <- names(objs)[duplicated(names(objs))]
      msg <- "`data` must contain must one definition per name"
      info <- sprintf("Found duplicate definitions for %s", collapse(dupes, quote = "`"))
      abort(c(msg, x = info), call = parent.frame())
    }
    short_nms <- sub("^[^:]+::", "", names(objs))
    dupes_lgl <- duplicated(short_nms) | duplicated(short_nms, fromLast = TRUE)
    names(objs)[!dupes_lgl] <- short_nms[!dupes_lgl]
  }
  objs
}

try_construct <- function(x, ...) {
  # deal early with special case x = quote(expr=)
  if (identical(x, quote(expr=))) return("quote(expr=)")
  caller <- caller_env()
  rlang::try_fetch(.cstr_construct(x, ...), error = function(e) {
    #nocov start
    abort("{constructive} could not build the requested code.", parent = e, call = caller)
    #nocov end
  })
}

try_parse <- function(code, one_liner) {
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
  # use local_bindings rather than `enclos =` so the expression is really evaled
  # in the proper env, this makes a difference for calls that capture the env
  local_bindings(!!!data, .env = caller)
  rlang::try_fetch(
    suppressWarnings(eval(parse(text = styled_code), caller)),
    error = function(e) {
      #nocov start
      msg <- "The code built by {constructive} could not be evaluated."
      if (isTRUE(check)) {
        print(styled_code)
        abort(msg, parent = e, call = caller)
      }
      # not sure if `e$message` can have length > 1 but playing safe
      rlang::inform(c("!" = msg, "!" = paste("Due to error:", paste(e$message, collapse = "\n"))))
      #nocov end
    }
  )
}

check_round_trip <- function(x, styled_code, data, check, compare, caller) {
  # return early if no check
  if (isFALSE(check)) return(NULL)

  # attempt to eval and fail explicitly if we can't
  evaled <- try_eval(styled_code, data, check, caller)
  if (missing(evaled) || (is.null(evaled) && !is.null(x))) return(NULL)

  # set custom method for waldo
  rlang::local_bindings(
    compare_proxy.ggplot = compare_proxy_ggplot,
    compare_proxy.weakref = compare_proxy_weakref,
    .env = .GlobalEnv)
  issues <-
    waldo::compare(
      x,
      evaled,
      x_arg = "original",
      y_arg = "recreated",
      ignore_srcref = compare$ignore_srcref,
      ignore_attr = compare$ignore_attr,
      ignore_encoding = TRUE,
      ignore_function_env = compare$ignore_function_env,
      ignore_formula_env = compare$ignore_formula_env,
      max_diffs = Inf
    )

  # return early if no issue
  if (!length(issues)) return(NULL)

  # set and signal issues
  globals$issues <- issues
  msg <- "{constructive} couldn't create code that reproduces perfectly the input"
  if (isTRUE(check)) {
    print(styled_code)
    msg <- paste0(msg, "\n", paste(issues, collapse = "\n"))
    abort(c(msg))
  }
  info <- "Call `construct_issues()` to inspect the last issues\n"
  rlang::inform(c(msg, i = info))

  # return issues
  issues
}

new_constructive <- function(code, compare) {
  structure(list(code = code, compare = compare), class = "constructive")
}

#' Generic for object code generation
#'
#' Exported for custom constructor design. `.cstr_construct()` is basically a
#' naked `construct()`, without the checks, the style, the object post processing etc...
#'
#' @inheritParams construct
#'
#' @return A character vector
#' @export
.cstr_construct <- function(x, ..., data = NULL) {
  data_name <- perfect_match(x, data)
  if (!is.null(data_name)) return(data_name)
  UseMethod(".cstr_construct")
}

#' Validate a constructor
#'
#' Fails if the chosen constructor doesn't exist.
#'
#' @param constructor a String (or character vector but only the first item will
#'   be considered)
#' @param class A string
#' @return A string, the first value of `constructor` if it is the name of a n existing
#' constructor or "next".
#' @export
.cstr_match_constructor <- function(constructor, class) {
  constructor <- constructor[[1]]
  choices <- ls(constructors[[class]], all.names = TRUE)
  internal_types <- c( # note: "..." replaced by "dots"
    "logical", "integer", "double", "complex", "character", "raw", "list", "NULL",
    "closure", "special", "builtin", "environment", "S4", "symbol", "pairlist",
    "promise", "language", "char", "any", "expression", "externalptr",
    "bytecode",  "weakref", "dots"
  )
  if (!class %in% internal_types) choices <- c(choices, "next")
  rlang::arg_match(constructor, choices)
  constructor
}

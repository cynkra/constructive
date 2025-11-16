#' Combine errors
#'
#' Exported for custom constructor design. This function allows combining independent checks so information is given about
#' all failing checks rather than the first one. All parameters except `...` are
#' forwarded to `rlang::abort()`
#'
#' @param ... check expressions
#' @param header An optional header to precede the errors
#' @inheritParams rlang::abort
#'
#' @return Returns `NULL` invisibly, called for side effects.
#' @export
.cstr_combine_errors <- function(
    ..., # unnamed expresions and named arg to forward to abort, such as `class`
    class = NULL,
    call,
    header = NULL,
    body = NULL,
    footer = NULL,
    trace = NULL,
    parent = NULL,
    use_cli_format = NULL,
    .internal = FALSE,
    .file = NULL,
    .frame = parent.frame(),
    .trace_bottom = NULL) {
  env <- parent.frame()
  dots <- eval(substitute(alist(...)))
  unnamed_dots <- dots[rlang::names2(dots) == ""]
  named_dots <- dots[rlang::names2(dots) != ""]
  named_dots <- eval(named_dots, env)
  err <- header
  for (expr in unnamed_dots) {
    err <- tryCatch({
      eval(expr, env)
      err
    }, error = function(e) {
      c(err, "!" = e$message, e$body)
    }
    )
  }
  if (!is.null(err)) {
    names(err)[1] <- ""
    do.call(rlang::abort, c(list(
      err,
      class = class,
      call = if (missing(call)) env else call,
      body = body,
      footer = footer,
      trace = trace,
      parent = parent,
      use_cli_format = use_cli_format,
      .internal = .internal,
      .file = .file,
      .frame = .frame,
      .trace_bottom = .trace_bottom
    ),
    named_dots))
  }
}

describe <- function(x) {
  type <- typeof(x)
  code <- construct(x, check = FALSE)$code
  code <- highlight_code(code)
  code <- paste(code, collapse = "\n")
  if (type %in% c("logical", "integer", "double", "complex", "character", "raw", "list")) {
    info <- sprintf("It has type '%s' and length %s:\n", typeof(x), length(x))
  } else {
    info <- sprintf("It has type '%s':\n", typeof(x))
  }
  paste0(info, code)
}

abort_not_boolean <- function(x) {
  var <- as.character(substitute(x))
  if (!rlang::is_bool(x)) {
    msg <- sprintf("`%s` is not a boolean (scalar `TRUE` or `FALSE`)", var)
    abort(c(msg, i = describe(x)), call = parent.frame())
  }
}

abort_not_string <- function(x) {
  var <- as.character(substitute(x))
  if (!rlang::is_string(x)) {
    msg <- sprintf("`%s` must be a string.", var)
    abort(c(msg, i = describe(x)), call = parent.frame())
  }
}

abort_not_null_or_integerish <- function(x) {
  var <- as.character(substitute(x))
  if (!rlang::is_null(x) && !rlang::is_integerish(x, 1)) {
    msg <- sprintf("`%s` is not `NULL` or a scalar integerish ", var)
    abort(c(msg, i = describe(x)), call = parent.frame())
  }
}

abort_not_env_or_named_list <- function(x) {
  var <- as.character(substitute(x))
  env_or_named_list_bool <-
    !is_environment(x) &&
    !(is_list(x) && is_named(x))
  if (env_or_named_list_bool) {
    msg <- sprintf("`%s` must be a named list or an environment.", var)
    info <- if (is_list(x)) {
      "It is a list with unnamed elements."
    } else {
      describe(x)
    }
    abort(c(msg, i = info), call = parent.frame())
  }
}

abort_wrong_data <- function(x) {
  if (is.null(x) || is.environment(x)) return(invisible(NULL))
  if (is.character(x)) {
    if (length(x) != 1) {
      msg <- "`data` has an unexpected value."
      info <- describe(x)
      abort(c(msg, i = info), call = parent.frame())
    }
    if (!is_installed(x)) {
      msg <- "`data` can be a string only if it's an installed package name."
      info <- sprintf("There is no installed package called '%s'", x)
      abort(c(msg, i = info), call = parent.frame())
    }
    return(invisible(NULL))
  }
  if (!is.environment(x) && !is.list(x)) {
    msg <- "`data` has an unexpected value."
    info <- describe(x)
    abort(c(msg, i = info), call = parent.frame())
  }
  nms <- names2(x)
  for (i in seq_along(x)) {
    if (nms[[i]] != "") next
    if (is.character(x[[i]])) {
      if (length(x[[i]]) != 1) {
        msg <- sprintf("`data[[%s]]` has an unexpected value", i)
        info <- describe(x[[i]])
        abort(c(msg, i = info), call = parent.frame())
      }
      if (!is_installed(x[[i]])) {
        msg <- "`data` can contain unnamed strings only if it's an installed package name."
        info1 <- sprintf("`data[[%s]]` is \"%s\".", i, x[[i]])
        info2 <- sprintf("There is no installed package called '%s'", x[[i]])
        abort(c(msg, i = info1, i = info2), call = parent.frame())
      }
      next
    }
    if (!is.environment(x[[i]]) && !(is.list(x[[i]]) && is_named2(x[[i]]))) {
      msg <- sprintf("`data[[%s]]` is unnamed and has an unexpected value.", i)
      info <- describe(x[[i]])
      abort(c(msg, i = info), call = parent.frame())
    }
  }
  invisible(NULL)
}

abort_self_reference <- function() {
  msg <- "The object contains self-references (environments depending pointing to themselves)"
  info <- "Consider using `opts_environment(\"predefine\")` or less reliably `recurse = FALSE`"
  rlang::abort(c(msg, i = info), call = parent.frame())
}

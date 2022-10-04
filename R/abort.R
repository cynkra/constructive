combine_errors <- function(
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
    new_err <- try(eval(expr, env), silent = TRUE)
    if (inherits(new_err, "try-error")) {
      err <- c(err, "!" = attr(new_err, "condition")$message)
    }
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
  pretty_code <- paste(prettycode::highlight(code), collapse = "\n")
  if (type %in% c("logical", "integer", "double", "complex", "character", "raw", "list")) {
    info <- sprintf("It has type '%s' and length %s:\n", typeof(x), length(x))
  } else {
    info <- sprintf("It has type '%s':\n", typeof(x))
  }
  paste0(info, pretty_code)
}

abort_not_boolean <- function(x) {
  var <- as.character(substitute(x))
  if (!rlang::is_bool(x)) {
    msg <- sprintf("`%s` is not a boolean (scalar `TRUE` or `FALSE`)", var)
    abort(c(msg, i = describe(x)), call = parent.frame())
  }
}

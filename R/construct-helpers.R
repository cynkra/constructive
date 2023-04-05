preprocess_data <- function(data, main = TRUE) {
  if (is.character(data) && length(data) == 1) return(namespace_as_list(data, main = main))
  if (is.environment(data)) return(as.list(data))
  # recurse into unnamed elements
  nms <- rlang::names2(data)
  named_elts <-  data[nms != ""]
  unnamed_elts <-  data[nms == ""]
  objs <- c(named_elts, do.call(c, lapply(unnamed_elts, preprocess_data, main = FALSE)))
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

  # compare, using use a temp ggplot method so we can have a fairer comparison
  issues <- with_s3_method(
    "compare_proxy", "ggplot", compare_proxy_ggplot, "waldo",
    waldo::compare(
      x,
      evaled,
      x_arg = "original",
      y_arg = "recreated",
      ignore_srcref = ignore_srcref,
      ignore_attr = ignore_attr,
      ignore_encoding = TRUE,
      ignore_function_env = ignore_function_env,
      ignore_formula_env = ignore_formula_env
    ))

  # return early if no issue
  if (!length(issues)) return(NULL)

  # set and signal issues
  globals$issues <- issues
  msg <- "{constructive} couldn't create code that reproduces perfectly the input"
  if (isTRUE(check)) {
    print(styled_code)
    msg <- paste0(msg, "\n", paste(out, collapse = "\n"))
    abort(c(msg))
  }
  info <- "Call `construct_issues()` to inspect the last issues"
  rlang::inform(c(msg, i = info))

  # return issues
  issues
}

construct_raw <- function(x, ..., data = NULL) {
  idiomatic_code <- data_match(x, data) %||% construct_idiomatic(x, ..., data = data)
  repaired_code <- repair_attributes(x, idiomatic_code, ..., data = data)
  repaired_code
}

data_match <- function(x, data) {
  if (is.null(data)) return(NULL)
  m <- match2(x, data)
  if (!length(m)) return(NULL)
  names(data)[m]
}

new_constructive <- function(code, compare) {
  structure(list(code = code, compare = compare), class = "constructive")
}

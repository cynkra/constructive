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
  if (identical(x, quote(expr=))) return("quote(expr = )")
  caller <- caller_env()
  rlang::try_fetch(.cstr_construct(x, ...), error = function(e) {
    #nocov start
    abort("{constructive} could not build the requested code.", parent = e, call = caller)
    #nocov end
  })
}

try_parse <- function(code, one_liner) {
  caller <- caller_env()
  rlang::try_fetch(
    rlang::parse_expr(paste0("{\n", paste(code, collapse = "\n"), "\n}\n")),
    error = function(e) {
      #nocov start
      abort("The code built by {constructive} could not be parsed.", parent = e, call = caller)
      #nocov end
    }
  )
  code <- as_constructive_code(code)
  code
}

try_eval <- function(styled_code, data, check, caller) {
  # use local_bindings rather than `enclos =` so the expression is really evaled
  # in the proper env, this makes a difference for calls that capture the env
  local_bindings(!!!data, .env = caller)
  rlang::try_fetch(
    suppress_all_output(eval(parse(text = styled_code), caller)),
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

suppress_all_output <- function(expr) {
  sink(tempfile())
  on.exit(sink())
  withCallingHandlers(
    expr,
    warning = function(w) tryInvokeRestart("muffleWarning"),
    message = function(c) tryInvokeRestart("muffleMessage")
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
    compare_proxy.R6ClassGenerator = compare_proxy_R6ClassGenerator,
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
.cstr_construct <- function(x, ..., data = NULL, classes = NULL) {
  data_name <- perfect_match(x, data)
  if (!is.null(data_name)) return(data_name)
  if (is.null(classes)) {
    UseMethod(".cstr_construct")
  } else if (identical(classes, "-")) {
    .cstr_construct.default(x, ..., classes = classes)
  } else if (classes[[1]] == "-") {
    cl <- setdiff(.class2(x), classes[-1])
    UseMethod(".cstr_construct", structure(NA_integer_, class = cl))
  } else {
    cl <- intersect(.class2(x), classes)
    UseMethod(".cstr_construct", structure(NA_integer_, class = cl))
  }
}

process_classes <- function(classes) {
  if (!length(classes)) return(NULL)
  classes <- setdiff(classes, "*none*")
  if ("*base*" %in% classes) {
    base_packages <- c("base", "utils", "stats", "methods", "grid")
    classes <- setdiff(c(classes, unlist(all_classes[base_packages])), "*base*")
  }
  exclude <- classes[startsWith(classes, "-")]
  include <- setdiff(classes, exclude)
  if (length(exclude)) {
    exclude <- sub("^-", "", exclude)
    packages_lgl <- grepl("^\\{.*\\}$", exclude)
    package_nms <- sub("^\\{(.*)\\}$", "\\1", exclude[packages_lgl])
    exclude <- unique(c(exclude[!packages_lgl], unlist(all_classes[package_nms])))
  }
  if (!length(include)) return(c("-", exclude))
  packages_lgl <- grepl("^\\{.*\\}$", include)
  package_nms <- sub("^\\{(.*)\\}$", "\\1", include[packages_lgl])
  include <- unique(c(all_classes[[1]], include[!packages_lgl], unlist(all_classes[package_nms])))
  include
}

# cat(sprintf('"%s"', sub("^opts_", "", ls(envir = asNamespace("constructive"), pattern = "^opts_"))), sep = ",\n")
all_classes <- list(
  c(
    ## low level classes that we can't remove
    "array",
    "character",
    "complex",
    "dots",
    "double",
    "environment",
    "expression",
    "externalptr",
    "function",
    "integer",
    "language",
    "list",
    "logical",
    "NULL",
    "pairlist",
    "raw",
    "S4",
    "weakref"
  ),
  base = c(
    "AsIs",
    "data.frame",
    "Date",
    "difftime",
    "error",
    "factor",
    "formula",
    "hexmode",
    "matrix",
    "noquote",
    "numeric_version",
    "octmode",
    "ordered",
    "package_version",
    "POSIXct",
    "POSIXlt",
    "R_system_version",
    "simpleCondition",
    "simpleError",
    "simpleMessage",
    "simpleWarning",
    "warning"
  ),
  utils = c(
    "bibentry",
    "citationFooter",
    "citationHeader",
    "person"
  ),
  stats = c(
    "mts",
    "ts"
  ),
  methods = c(
    "classGeneratorFunction",
    "classPrototypeDef",
    "classRepresentation"
  ),
  bit64 = c("integer64"),
  blob = c(
    "blob"
  ),
  ggplot2 = c(
    "CoordCartesian",
    "CoordFixed",
    "CoordFlip",
    "CoordMap",
    "CoordMunch",
    "CoordPolar",
    "CoordQuickmap",
    "CoordSf",
    "CoordTrans",
    "element_blank",
    "element_grob",
    "element_line",
    "element_rect",
    "element_render",
    "element_text",
    "FacetWrap",
    "ggplot",
    "ggproto",
    "labels",
    "Layer",
    "margin",
    "rel",
    "Scale",
    "ScalesList",
    "theme",
    "uneval",
    "waiver"
  ),
  data.table = c("data.table"),
  dm = c("dm"),
  dplyr = c(
    "grouped_df",
    "rowwise_df"
  ),
  grid = c(
    "simpleUnit"
  ),
  rlang = c(
    "quosure",
    "quosures"
  ),
  tibble = c("tbl_df"),
  vctrs = c("vctrs_list_of")
)

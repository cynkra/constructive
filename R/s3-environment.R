#' Constructive options for type 'environment'
#'
#' Environments use reference semantics, they cannot be copied.
#' An attempt to copy an environment would indeed yield a different environment and `identical(env, copy)` would be `FALSE`.\cr
#' Moreover most environments have a parent (exceptions are `emptyenv()` and some
#' rare cases where the parent is `NULL`) and thus to copy the environment we'd
#' have to have a way to point to the parent, or copy it too. \cr
#' For this reason environments are \pkg{constructive}'s cryptonite. They make some objects
#' impossible to reproduce exactly. And since every function or formula has one they're hard to
#' avoid. \cr
#'
#' @details
#' In some case we can build code that points to a specific environment, namely:
#' * `.GlobalEnv`, `.BaseNamespaceEnv`, `baseenv()` and `emptyenv()` are used to construct
#'   the global environment, the base namespace, the base package environment and the empty
#'   environment
#' * Namespaces are constructed using `asNamespace("pkg")`
#' * Package environments are constructed using `as.environment("package:pkg")`
#' * "imports" environments are constructed with `parent.env(asNamespace("pkg"))`
#' * "lazydata" environments are constructed with `getNamespaceInfo("pkg", "lazydata")`
#'
#' By default For other environments we use \pkg{constructive}'s function `constructive::.env()`, it fetches
#'   the environment from its memory address and provides as additional information
#'   the sequence of parents until we reach a special environment (those enumerated above).
#'   The advantage of this approach is that it's readable and that the object is accurately reproduced.
#'   The inconvenient is that it's not stable between sessions. If an environment has a `NULL` parent it's always constructed
#'   with `constructive::.env()`, whatever the choice of the constructor.
#'
#' Often however we wish to be able to reproduce from scratch a similar environment,
#' so that we might run the constructed code later in a new session. We offer different
#' different options to do this, with different trade-offs regarding accuracy and verbosity.
#'
#' \{constructive\} will not signal any difference if it can reproduce an equivalent environment,
#' defined as containing the same values and having a same or equivalent parent.\cr
#'
#' See also the `ignore_function_env` argument in `?compare_options`, which disables the check
#' of environments of function.
#'
#' @section Constructors:
#'
#' We might set the `constructor` argument to:
#'
#' - `".env"` (default): use `constructive::.env()` to construct the environment from
#'   its memory address. Additional information is displayed through its
#'   non-functional arguments, see the `context` argument.
#' * `"list2env"`: We construct the environment as a list then
#'   use `base::list2env()` to convert it to an environment and assign it a parent. By
#'   default we use as a parent the first special environment we find when going
#'   through ancestors, so we can print code that doesn't use `.env()`.
#'   If `recurse` is `TRUE`
#'   the parent will be built recursively so all ancestors will be created until
#'   we meet a known environment, this might be verbose and will fail if environments
#'   are nested too deep or have a circular relationship. If the environment is empty we use `new.env(parent=)`
#'   for a more economic syntax.
#' * `"new_environment"` : Similar to the above, but using `rlang::new_environment()`.
#' * `"new.env"` : All environments will be recreated with the code `"base::new.env()"`,
#'   without argument, effectively creating an empty environment child of
#'   the local (often global) environment. This is enough in cases where the environment
#'   doesn't matter (or matters as long as it inherits from the local environment),
#'   as is often the case with formulas. `recurse` is ignored.
#' * `"as.environment"` : we attempt to construct the environment as a list and use
#' `base::as.environment()` on top of it, as in `as.environment(list(a=1, b=2))`, it will
#'  contain the same variables as the original environment but the parent will be the
#'  `emptyenv()`. `recurse` is ignored.
#' * `"topenv"` : we construct `base::topenv(x)`, see `?topenv`. `recurse` is ignored.
#'   This is the most accurate we can be when constructing only special environments.
#' * `"predefine"` : Building environments from scratch using the above methods
#'   can be verbose, sometimes redundant and sometimes even impossible due to
#'   circularity (e.g. an environment referencing itself).  With `"predefine"`
#'   we define the environments and their content  above the object returning
#'   call, using placeholder names `..env.1..`, `..env.2..` etc.
#'   The caveat is that the created code won't be a single call
#'   and will create objects in the workspace. `recurse` is ignored.
#' @param constructor String. Name of the function used to construct the
#'   environment, see **Constructors** section.
#' @inheritParams opts_atomic
#' @param recurse Boolean. Only considered if `constructor` is `"list2env"` or
#'   `"new_environment"`. Whether to attempt to recreate all parent environments
#'   until a known environment is found, if `FALSE` (the default) we will use
#'    `topenv()` to find a known ancestor to set as the parent.
#' @param context Character vector. Only considered if `constructor` is `".env"`.
#'   Information to display as arguments of `.env()`, they are ignored by the
#'   function and don't change the constructed object. A subset of:
#'   * `"parents"`: the names or memory addresses of the parent environments,
#'     until a special environment is found
#'   * `"attributes"`: the attributes of the environment
#'   * `"locked"`: `locked = TRUE` if the environment is locked
#'   * `"names"`: the names of the bindings
#'   * `"objects"`: the construction of the bindings, active and lazy bindings
#'     are omitted so they're not evaluated
#'   * `"parent"`: the construction of the parent environment, repeated until a
#'     special environment is found
#'
#'   Environments found in `"objects"` and `"parent"` don't display objects, and
#'   environments found in `"objects"` don't display their parent.
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_environment <- function(constructor = c(".env", "list2env", "as.environment", "new.env", "topenv", "new_environment", "predefine"), ..., recurse = FALSE, context = c("parents", "attributes", "locked")) {
  if (isTRUE(list(...)$predefine)) {
    msg <- "`predefine = TRUE` in `opts_environment()` is deprecated"
    info <- "Use `constructor = \"predefine\"` instead."
    rlang::warn(c(msg, i = info))
    constructor <- "predefine"
  }
  .cstr_combine_errors(
    abort_not_boolean(recurse),
    if (!is.character(context) || anyNA(context) || !all(context %in% env_context_values)) {
      abort(c(
        "`context` must be a character vector containing only the following values:",
        paste(sprintf('"%s"', env_context_values), collapse = ", ")
      ))
    }
  )
  .cstr_options("environment", constructor = constructor[[1]], ..., recurse = recurse, context = context)
}

env_context_values <- c("parents", "attributes", "locked", "names", "objects", "parent")

#' @export
#' @method .cstr_construct environment
.cstr_construct.environment <- function(x, ...) {
  # The name of `asNamespace("pkg")` is always "pkg" and print as `<environment: namespace:pkg>`
  # The name of `as.environment("package:pkg")` is ALMOST always "package:pkg" and prints as
  #  `<environment: package:pkg>` + attributes
  # The exception is `as.environment("package:base")` which prints as
  #   `<environment: base>` and whose name is "base"
  # This means `asNamespace("base")` (a.k.a. `.BaseNamespaceEnv`) and
  #   `as.environment("package:base")` (a.k.a. `baseenv()`) have the same name
  #   but are different. So we implement a workaround.
  opts <- list(...)$opts$environment %||% opts_environment()
  if (is_corrupted_environment(x)) return(NextMethod())

  # if we can match a special env, return it directly
  code <- construct_special_env(x)
  if (!is.null(code)) return(code)

  if (is.null(parent.env(x))) {
    # according to error of `new.env(parent = NULL)` we should not find NULL
    # parents anymore, yet we do. In this case we force the use of `env` as a constructor
    # because it's the only one that can reproduce these objects.
    .cstr_construct.environment..env(x, ...)
  } else {
    UseMethod(".cstr_construct.environment", structure(NA, class = opts$constructor))
  }
}

is_corrupted_environment <- function(x) {
  !is.environment(x)
}

#' @export
#' @method .cstr_construct.environment predefine
.cstr_construct.environment.predefine <- function(x, ...) {
  update_predefinition(envir = x, ...)
}

#' @export
#' @method .cstr_construct.environment .env
.cstr_construct.environment..env <- function(x, ...) {
  opts <- list(...)$opts$environment %||% opts_environment()
  context <- opts$context %||% c("parents", "attributes", "locked")
  args <- list(env_memory_address(x))
  if ("parents" %in% context) {
    parents <- fetch_parent_names(x)
    if (length(parents)) args$parents <- parents
  }
  if ("names" %in% context) {
    args$names <- sort(ls(x, all.names = TRUE, sorted = FALSE), method = "radix")
  }
  if ("attributes" %in% context) args <- c(args, attributes(x))
  if ("locked" %in% context && environmentIsLocked(x)) args$locked <- TRUE
  code <- lapply(args, function(arg) .cstr_construct(arg, ...))

  # objects and the parent are constructed as information, environments
  # that they contain don't display objects and parents, to avoid infinite
  # recursion
  if (any(c("objects", "parent") %in% context)) {
    dots <- list(...)
    nested_opts <- opts
    nested_opts$context <- setdiff(context, c("objects", "parent"))
    dots$opts$environment <- nested_opts
    if ("objects" %in% context) {
      code$objects <- do.call(.cstr_construct, c(list(env_regular_bindings(x)), dots))
    }
    if ("parent" %in% context && !is.null(parent.env(x))) {
      # the parent's own parent is displayed too, until we reach a special env
      parent_dots <- list(...)
      parent_opts <- opts
      parent_opts$context <- setdiff(context, "objects")
      parent_dots$opts$environment <- parent_opts
      code$parent <- do.call(.cstr_construct, c(list(parent.env(x)), parent_dots))
    }
    # keep "locked" and the attributes last
    first <- intersect(c("", "parents", "names", "objects", "parent"), names2(code))
    code <- code[c(which(names2(code) %in% first), which(!names2(code) %in% first))]
  }
  code <- .cstr_apply(code, "constructive::.env", ..., recurse = FALSE)
  repair_attributes_environment(x, code, ...)
}

# list of the bindings that are not active or lazy, so we don't trigger side
# effects when displaying them
env_regular_bindings <- function(x) {
  nms <- sort(ls(x, all.names = TRUE, sorted = FALSE), method = "radix")
  nms <- nms[!vapply(nms, bindingIsActive, logical(1), x)]
  nms <- nms[!rlang::env_binding_are_lazy(x, nms)]
  mget(nms, envir = x)
}

#' @export
#' @method .cstr_construct.environment list2env
.cstr_construct.environment.list2env <- function(x, ...) {
  opts <- list(...)$opts$environment %||% opts_environment()
  if (contains_self_reference(
    x,
    check_parent = opts$recurse,
    check_function_env = list(...)$opts$`function`$environment %||% TRUE,
    # FIXME: this would be a very contrived corner case to have a corrupted
    #   srcref containing environment self refs
    check_srcref = FALSE # list(...)$opts$`function`$srcref %||% FALSE
    )) {
    abort_self_reference()
  }

  if (!opts$recurse) {
    parent_code <- construct_top_env(parent.env(x))
    if (length(names(x))) {
      list_code <- .cstr_construct(env2list(x), ...)
      code <- .cstr_apply(
        list(list_code, parent = parent_code),
        "list2env",
        ...,
        recurse = FALSE
      )
      code <- apply_env_locks(x, code, ...)
      return(repair_attributes_environment(x, code, ...))
    }
    code <- .cstr_apply(list(parent = parent_code), "new.env", ..., recurse = FALSE)
    code <- apply_env_locks(x, code, ...)
    return(repair_attributes_environment(x, code, ...))
  }

  placeholder <- get_pipe_placeholder(list(...)$pipe)
  lhs_code <- .cstr_construct(parent.env(x), ...)
  if (length(names(x))) {
    data_code <- .cstr_construct(env2list(x), ...)
    rhs_code <- .cstr_apply(list(data_code, parent = placeholder), "list2env", ..., recurse = FALSE)
    code <- .cstr_pipe(lhs_code, rhs_code, ...)
  } else {
    rhs_code <- .cstr_apply(list(parent = placeholder), "new.env", ..., recurse = FALSE)
    code <- .cstr_pipe(lhs_code, rhs_code, ...)
  }
  code <- apply_env_locks(x, code)
  repair_attributes_environment(x, code, ...)
}

#' @export
#' @method .cstr_construct.environment new_environment
.cstr_construct.environment.new_environment <- function(x, ...) {
  opts <- list(...)$opts$environment %||% opts_environment()
  if (contains_self_reference(
    x,
    check_parent = opts$recurse,
    check_function_env = list(...)$opts$`function`$environment %||% TRUE,
    # FIXME: this would be a very contrived corner case to have a corrupted
    #   srcref containing environment self refs
    check_srcref = FALSE # list(...)$opts$`function`$srcref %||% FALSE
  )) {
    abort_self_reference()
  }
  if (!opts$recurse) {
    parent_code <- construct_top_env(parent.env(x))
    if (length(names(x))) {
      list_code <- .cstr_construct(env2list(x), ...)
      code <- .cstr_apply(
        list(list_code, parent = parent_code),
        "rlang::new_environment",
        ...,
        recurse = FALSE
      )
      code <- apply_env_locks(x, code)
      return(repair_attributes_environment(x, code, ...))
    }
    code <- .cstr_apply(list(parent = parent_code), "rlang::new_environment", ...)
    code <- apply_env_locks(x, code)
    return(repair_attributes_environment(x, code, ...))
  }

  placeholder <- get_pipe_placeholder(list(...)$pipe)
  lhs_code <- .cstr_construct(parent.env(x), ...)
  if (length(names(x))) {
    data_code <- .cstr_construct(env2list(x), ...)
    rhs_code <- .cstr_apply(list(data_code, parent = placeholder), "rlang::new_environment", ..., recurse = FALSE)
    code <- .cstr_pipe(lhs_code, rhs_code, ...)
  } else {
    rhs_code <- .cstr_apply(list(parent = placeholder), "rlang::new_environment", ..., recurse = FALSE)
    code <- .cstr_pipe(lhs_code, rhs_code, ...)
  }
  code <- apply_env_locks(x, code)
  repair_attributes_environment(x, code, ...)
}

#' @export
#' @method .cstr_construct.environment new.env
.cstr_construct.environment.new.env <- function(x, ...) {
  code <- "new.env()"
  repair_attributes_environment(x, code, ...)
}

#' @export
#' @method .cstr_construct.environment as.environment
.cstr_construct.environment.as.environment <- function(x, ...) {
  if (contains_self_reference(
    x,
    check_parent = FALSE,
    check_function_env = list(...)$opts$`function`$environment %||% TRUE,
    # FIXME: this would be a very contrived corner case to have a corrupted
    #   srcref containing environment self refs
    check_srcref = FALSE # list(...)$opts$`function`$srcref %||% FALSE
  )) {
    abort_self_reference()
  }
  # We need to use as.list.environment() (via env2list()) because as.list() will only map
  # to as.list.environment() if class was not overriden
  code <- .cstr_wrap(
    .cstr_construct(env2list(x), ...),
    "as.environment",
    new_line = FALSE
  )
  code <- apply_env_locks(x, code)
  repair_attributes_environment(x, code, ...)
}

#' @export
#' @method .cstr_construct.environment topenv
.cstr_construct.environment.topenv <- function(x, ...) {
  code <- .cstr_construct(topenv(x), ...)
  code
}

repair_attributes_environment <- function(x, code, ...) {
  opts <- list(...)$opts$environment %||% opts_environment()
  if (opts$constructor == ".env" ||
      grepl("^asNamespace\\(\"[^\"]+\"\\)", code[[1]]) ||
      code[[1]] %in% c("baseenv()", "emptyenv()", ".GlobalEnv", ".BaseNamespaceEnv")
  ) {
    # nothing to repair
    return(code)
  }

  pkg_env_lgl <- grepl("as.environment\\(\"[^\"]+\"\\)", code[[1]])
  .cstr_repair_attributes(
    x, code, ...,
    ignore = c(
      # pkg:fun envs have name and path attributes already set by `as.environment()`
      if (pkg_env_lgl) c("name", "path"),
      if (opts$constructor == "predefine") "class"
    )
  )
}

env2list <- function(x) {
  as.list.environment(x, all.names = TRUE, sorted = TRUE)
}

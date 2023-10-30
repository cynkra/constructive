# see also environment_utils.R
constructors$environment <- new.env()

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
#'   its memory address.
#' * `"list2env"`: We construct the environment as a list then
#'   use `base::list2env()` to convert it to an environment and assign it a parent. By
#'   default we will use `base::topenv()` to construct a parent. If `recurse` is `TRUE`
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
#'
#' @section Predefine:
#'
#'  Building environments from scratch using the above methods can be verbose and
#' sometimes redundant if and environment is used several times. One last option
#' is to define the environments and their content above the object returning call,
#' using placeholder names `..env.1..`, `..env.2..` etc. This is done by setting
#' `predefine` to `TRUE`. `constructor` and `recurse` are ignored in that case.
#'
#' @param constructor String. Name of the function used to construct the environment, see **Constructors** section.
#' @inheritParams opts_atomic
#' @param recurse Boolean. Only considered if `constructor` is `"list2env"` or `"new_environment"`. Whether to
#'   attempt to recreate all parent environments until a known environment is found,
#'   if `FALSE` (the default) we will use `topenv()` to find a known ancestor to set as
#'   the parent.
#' @param predefine Boolean. Whether to define environments first. If `TRUE` `constructor` and `recurse`
#'   are ignored. It circumvents the circularity, recursivity and redundancy issues of
#'   other constructors. The caveat is that the created code won't be a single call
#'   and will create objects in the workspace.
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_environment <- function(constructor = c(".env", "list2env", "as.environment", "new.env", "topenv", "new_environment"), ..., recurse = FALSE, predefine = FALSE) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "environment"),
    ellipsis::check_dots_empty(),
    abort_not_boolean(recurse)
  )
  .cstr_options("environment", constructor = constructor, recurse = recurse, predefine = predefine)
}

#' @export
.cstr_construct.environment <- function(x, ..., pipe = NULL, one_liner = FALSE) {
  # The name of `asNamespace("pkg")` is always "pkg" and print as `<environment: namespace:pkg>`
  # The name of `as.environment("package:pkg")` is ALMOST always "package:pkg" and prints as
  #  `<environment: package:pkg>` + attributes
  # The exception is `as.environment("package:base")` which prints as
  #   `<environment: base>` and whose name is "base"
  # This means `asNamespace("base")` (a.k.a. `.BaseNamespaceEnv`) and
  #   `as.environment("package:base")` (a.k.a. `baseenv()`) have the same name
  #   but are different. So we implement a workaround.
  opts <- .cstr_fetch_opts("environment", ...)
  if (is_corrupted_environment(x)) return(NextMethod())

  # if we can match a special env, return it directly
  code <- construct_special_env(x)
  if (!is.null(code)) return(code)

  null_parent <- is.null(parent.env(x))
  # FIXME: what does this do ?
  if (opts$predefine && !null_parent) {
    special_envs <-  c(search(), "R_EmptyEnv", "R_GlobalEnv")
    nm <- environmentName(x)
    # construct only if not found
    if (!(nm != "" && rlang::is_installed(nm)) && ! nm %in% globals$special_envs) {
      code <- update_predefinition(envir = x, ..., pipe = pipe, one_liner = one_liner)
      return(code)
    }
  }

  if (null_parent) {
    # according to error of `new.env(parent = NULL)` we should nopt find NULL
    # parents anymore, yet we do. In this case we force the use of `env` as a constructor
    # because it's the only one that can reproduce these objects.
    constructor <- constructors$environment[["env"]]
  } else {
    constructor <- constructors$environment[[opts$constructor]]
  }

  constructor(x, ..., pipe = pipe, one_liner = one_liner, recurse = opts$recurse, predefine = opts$predefine)
}

is_corrupted_environment <- function(x) {
  !is.environment(x)
}

constructors$environment$.env <- function(x, ..., pipe, one_liner, recurse, predefine) {
  args <- c(
    list(env_memory_address(x), parents = fetch_parent_names(x)),
    attributes(x)
  )
  if (!length(args$parents)) args$parents <- NULL
  code <- .cstr_apply(args, "constructive::.env", ..., pipe = pipe, one_liner = one_liner)
  repair_attributes_environment(x, code, ...)
}

constructors$environment$list2env <- function(x, ..., pipe, one_liner, recurse, predefine) {
  if (is.null(pipe)) {
    if (getRversion() >= "4.2") {
      pipe <- "base"
    } else {
      pipe <- "magrittr"
    }
  } else {
    pipe <- rlang::arg_match(pipe, c("base", "magrittr"))
  }

    if (!recurse) {
      if (length(names(x))) {
        code <- .cstr_apply(list(env2list(x), parent = topenv(x)), "list2env", ..., pipe = pipe, one_liner = one_liner)
        return(repair_attributes_environment(x, code, ...))
      }
      code <- .cstr_apply(list(parent = topenv(x)), "new.env", ..., pipe = pipe, one_liner = one_liner)
      return(repair_attributes_environment(x, code, ...))
    }

    placeholder <- get_pipe_placeholder(pipe)
    lhs_code <- .cstr_construct(parent.env(x), ..., pipe = pipe, one_liner = one_liner)
    if (length(names(x))) {
      data_code <- .cstr_construct(env2list(x), ..., pipe = pipe, one_liner = one_liner)
      rhs_code <- .cstr_apply(list(data_code, parent = placeholder), "list2env", ..., recurse = FALSE, pipe = pipe, one_liner = one_liner)
      code <- .cstr_pipe(lhs_code, rhs_code, pipe = pipe, one_liner = one_liner)
    } else {
      rhs_code <- .cstr_apply(list(parent = placeholder), "new.env", ..., recurse = FALSE, pipe = pipe, one_liner = one_liner)
      code <- .cstr_pipe(lhs_code, rhs_code, pipe = pipe, one_liner = one_liner)
    }

  repair_attributes_environment(x, code, ...)
}

constructors$environment$new_environment <- function(x, ..., pipe, one_liner, recurse, predefine) {
  if (is.null(pipe)) {
    if (getRversion() >= "4.2") {
      pipe <- "base"
    } else {
      pipe <- "magrittr"
    }
  } else {
    pipe <- rlang::arg_match(pipe, c("base", "magrittr"))
  }

  if (!recurse) {
    if (length(names(x))) {
      code <- .cstr_apply(list(env2list(x), parent = topenv(x)), "rlang::new_environment", ..., pipe = pipe, one_liner = one_liner)
      return(repair_attributes_environment(x, code, ...))
    }
    code <- .cstr_apply(list(parent = topenv(x)), "rlang::new_environment", ..., pipe = pipe, one_liner = one_liner)
    return(repair_attributes_environment(x, code, ...))
  }

  placeholder <- get_pipe_placeholder(pipe)
  lhs_code <- .cstr_construct(parent.env(x), ..., pipe = pipe, one_liner = one_liner)
  if (length(names(x))) {
    data_code <- .cstr_construct(env2list(x), ..., pipe = pipe, one_liner = one_liner)
    rhs_code <- .cstr_apply(list(data_code, parent = placeholder), "rlang::new_environment", ..., recurse = FALSE, pipe = pipe, one_liner = one_liner)
    code <- .cstr_pipe(lhs_code, rhs_code, pipe = pipe, one_liner = one_liner)
  } else {
    rhs_code <- .cstr_apply(list(parent = placeholder), "rlang::new_environment", ..., recurse = FALSE, pipe = pipe, one_liner = one_liner)
    code <- .cstr_pipe(lhs_code, rhs_code, pipe = pipe, one_liner = one_liner)
  }
  repair_attributes_environment(x, code, ..., pipe = pipe, one_liner = one_liner)
}

constructors$environment$new.env <- function(x, ..., pipe, one_liner, recurse, predefine) {
  code <- "new.env()"
  repair_attributes_environment(x, code, ...)
}

constructors$environment$as.environment <- function(x, ..., pipe, one_liner, recurse, predefine) {
  # We need to use as.list.environment() (via env2list()) because as.list() will only map
  # to as.list.environment() if class was not overriden
  code <- .cstr_wrap(
    .cstr_construct(env2list(x), ...),
    "as.environment",
    new_line = FALSE
  )
  repair_attributes_environment(x, code, ...)
}

constructors$environment$topenv <- function(x, ..., pipe, one_liner, recurse, predefine) {
  code <- .cstr_construct(topenv(x), ...)
  code
}

repair_attributes_environment <- function(x, code, ..., pipe = NULL) {
  opts <- .cstr_fetch_opts("environment", ...)
  constructor <- opts$constructor
  if (constructor == "env" ||
      grepl("^asNamespace\\(\"[^\"]+\"\\)", code[[1]]) ||
      code[[1]] %in% c("baseenv()", "emptyenv()", ".GlobalEnv", ".BaseNamespaceEnv")
  ) {
    # nothing to repair
    return(code)
  }

  pkg_env_lgl <- grepl("as.environment\\(\"[^\"]+\"\\)", code[[1]])
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = c(
      # pkg:fun envs have name and path attributes already set by `as.environment()`
      if (pkg_env_lgl) c("name", "path"),
      if (opts$predefine) "class"
    )
  )
}

env2list <- function(x) {
  as.list.environment(x, all.names = TRUE, sorted = TRUE)
}

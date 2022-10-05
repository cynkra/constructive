#' Constructive options for type 'environment'
#'
#' @details
#' Environments use reference semantics, they cannot be copied.
#' An attempt to copy an environment would indeed yield a different environment and `identical(env, copy)` would be `FALSE`.\cr
#' Moreover every environment (except for `emptyenv()`) has a
#' parent and thus to copy the environment we'd have to have a way to point to
#' the parent, or copy it too.\cr
#' For this reason environments are {constructive}'s cryptonite. They make some objects
#' impossible to reproduce exactly. And since every function or formula has one they're hard to
#' avoid \cr
#' Luckily in many cases we don't need a perfect copy of an environment to reproduce
#' an object faithfully from a practical standpoint, and in a some cases we might build
#' code that points to a specific environment such as `.GlobalEnv` or a given namespace.\cr
#' See also the `ignore_function_env` argument in `?construct`, which disables the check
#' of environments of function.
#'
#' Environment construction works as follows:
#' * Special environments `.GlobalEnv`, `.BaseNamespaceEnv`, `baseenv()` and
#'   `emptyenv()` are used whenever possible.
#' * The idiom `asNamespace("pkg")` is
#'   used whenever possible.
#' * The idiom `as.environment("package:pkg")` is used
#'   whenever possible.
#' * The `constructor` argument is used to recreate a similar environment whenever
#'   the above conditions are not met.
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"list2env"` (default): We construct the environment as a list then
#'   use `base::list2env()` to convert it to an environment and assign it a parent. By
#'   default we will use `base::topenv()` to construct a parent. If `recurse` is `TRUE`
#'   the parent will be built recursively so all ancestors will be created until
#'   we meet a known environment, this might be verbose and will fail if environments
#'   are nested too deep or have a circular relationship, but if it works it's the
#'   most faithful method. If the environment is empty we use `new.env(parent=)`
#'   for a more economic syntax.
#' * `"new_environment"` : Similar to the above, but using `rlang::new_environment()`.
#' * `"new.env"` : All environments will be recreated with the code `"base::new.env()"`, effectively creating an empty environment child of
#'   the local (often global) environment. This is enough in cases where the environment
#'   doesn't matter at all (or matters as long as it inherits from the local environment),
#'   this is the case for most formulas. `recurse` is ignored.
#' * `"as.environment"` : we attempt to construct the environment as a list and use
#' `base::as.environment()` on top of it, as in `as.environment(list(a=1, b=2))`, it will
#'  contain the same variables as the original environment but the parent will be the
#'  `emptyenv()`. `recurse` is ignored.
#' * `"topenv"` : we construct `base::topenv(x)`, see `?topenv`. `recurse` is ignored.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @param recurse Boolean. Only considered if `constructor` is `"list2env"` or `"new_environment"`. Whether to
#'   attempt to recreate all parent environments until a known environment is found,
#'   if `FALSE` (the default) we will use `topenv()` to find a known ancestor to set as
#'   the parent.
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_environment <- function(constructor = c("list2env", "as.environment", "new.env", "topenv", "new_environment"), recurse = FALSE) {
  constructor <- rlang::arg_match(constructor)
  abort_not_boolean(recurse)
  structure(
    class = c("constructive_options", "constructive_options_environment"),
    list(constructor = constructor, recurse = recurse)
  )
}


#' @export
construct_idiomatic.environment <- function(x, ..., pipe = "base", one_liner = FALSE) {
  # The name of `asNamespace("pkg")` is always "pkg" and print as `<environment: namespace:pkg>`
  # The name of `as.environment("package:pkg")` is ALMOST always "package:pkg" and prints as
  #  `<environment: package:pkg>` + attributes
  # The exception is `as.environment("package:base")` which prints as
  #   `<environment: base>` and whose name is "base"
  # This means `asNamespace("base")` (a.k.a. `.BaseNamespaceEnv`) and
  #   `as.environment("package:base")` (a.k.a. `baseenv()`) have the same name
  #   but are different. So we implement a workaround.
  args <- fetch_opts("environment", ...)
  if (identical(x, baseenv())) return('baseenv()')
  if (identical(x, emptyenv())) return('emptyenv()')
  name <- environmentName(x)
  if (name == "R_GlobalEnv") return(".GlobalEnv")
  if (name == "base") return(".BaseNamespaceEnv")
  if (name %in% row.names(installed.packages())) return(sprintf('asNamespace("%s")', name))
  if (name %in% search()) return(sprintf('as.environment("%s")', name))

  if (args$constructor %in% c("list2env", "new_environment")) {
    constructor <- switch(
      args$constructor,
      list2env = "list2env",
      new_environment = "rlang::new_environment"
    )

    if (!args$recurse) {
      if (length(names(x))) {
        code <- construct_apply(list(as.list.environment(x), parent = topenv(x)), constructor, ..., pipe = pipe, one_liner = one_liner)
        return(code)
      }
      if (constructor == "list2env") constructor <- "new.env"
      code <- construct_apply(list(parent = topenv(x)), constructor, ..., pipe = pipe, one_liner = one_liner)
      return(code)
    }

    place_holder = c(base = "_", magrittr = ".")[pipe]
    lhs_code <- construct_raw(parent.env(x), ..., pipe = pipe, one_liner = one_liner)
    if (length(names(x))) {
      data_code <- construct_raw(as.list.environment(x), ..., pipe = pipe, one_liner = one_liner)
      rhs_code <- construct_apply(list(data_code, parent = place_holder), constructor, ..., language = TRUE, pipe = pipe, one_liner = one_liner)
      code <- pipe(lhs_code, rhs_code, pipe = pipe, one_liner = one_liner)
    } else {
      if (constructor == "list2env") constructor <- "new.env"
      rhs_code <- construct_apply(list(parent = place_holder), constructor, ..., language = TRUE, pipe = pipe, one_liner = one_liner)
      code <- pipe(lhs_code, rhs_code, pipe = pipe, one_liner = one_liner)
    }
    return(code)
  }

  if (args$constructor == "new.env") return("new.env()")

  if (args$constructor == "as.environment") {
    # We need to use as.list.environment directly because as.list will only map
    # to "as.list.environment" if class was not overriden
    code <- wrap(
      construct_raw(as.list.environment(x), ...),
      "as.environment",
      new_line = FALSE
    )
    return(code)
  }

  # if (args$constructor == "topenv")
  construct_raw(topenv(x))
}

#' @export
repair_attributes.environment <- function(x, code, ..., pipe ="base") {
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = c("name", "path")
  )
}

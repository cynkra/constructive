constructors$formula <- new.env()

#' Constructive options for formulas
#'
#' These options will be used on formulas, defined as calls to `~`, regardless
#' of their `"class"` attribute.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @param environment Boolean. Whether to attempt to construct the environment,
#'   by default `FALSE` for the `"~"` comstructor and `TRUE` for the others
#'
#' Depending on `constructor`, we construct the formula as follows:
#' * `"~"` (default): We construct the formula in the most common way using the `~`
#'   operator. By default this sets the formula's environment to the local environment.
#'   Setting `environment = TRUE` will be have the environment set by a `structure(, .Environment=)`
#'   call.
#' * `"formula"` : deparse the formula as a string and use `base::formula()` on top of it,
#'   if `environment = TRUE` (the default), we will attempt to construct the
#'   environment and provide it as the `env` argument.
#' * `"as.formula"` : Same as above, but using `base::as.formula()`.
#' * `"new_formula"` : extract both sides of the formula as separate language objects
#'   and feed them to `rlang::new_formula()`, along with the reconstructed environment
#'   if `environment` is `TRUE` (the default)
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_formula <- function(constructor = c("~", "formula", "as.formula", "new_formula"), ..., environment = constructor != "~") {
  combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty(),
    abort_not_boolean(environment)
  )
  constructive_options("formula", constructor = constructor, environment = environment)
}

#' @export
construct_raw.formula <- function(x, ...) {
  opts <- fetch_opts("formula", ...)
  if (is_corrupted_formula(x)) return(NextMethod())
  constructor <- constructors$formula[[opts$constructor]]
  constructor(x, ..., environment = opts$environment)
}

#' @export
is_corrupted_formula <- function(x) {
  !is.call(x) || !identical(.subset2(x, 1), quote(`~`))
}

constructors$formula$"~" <- function(x, ..., environment) {
  code <- deparse(x)
  repair_attributes.formula(x, code, ...)
}

constructors$formula$new_formula <- function(x, ..., environment) {
  lhs_code <- construct_raw(rlang::f_lhs(x), ...)
  rhs_code <- construct_raw(rlang::f_rhs(x), ...)
  if (environment) {
    env_code <- construct_raw(environment(x), ...)
    code <- construct_apply(list(lhs_code, rhs_code, env = env_code), "rlang::new_formula", ..., language = TRUE)
  } else {
    code <- construct_apply(list(lhs_code, rhs_code), "rlang::new_formula", ..., language = TRUE)
  }
  repair_attributes.formula(x, code, ...)
}

constructors$formula$formula <- function(x, ..., environment) {
  if (environment) {
    code <- construct_apply(list(deparse(x), env = environment(x)), "formula", ...)
  } else {
    code <- construct_apply(list(deparse(x)), "formula", ..., language = TRUE)
  }
  repair_attributes.formula(x, code, ...)
}

constructors$formula$as.formula <- function(x, ..., environment) {
  if (environment) {
    code <- construct_apply(list(deparse(x), env = environment(x)), "as.formula", ...)
  } else {
    code <- construct_apply(list(deparse(x)), "as.formula", ..., language = TRUE)
  }
  repair_attributes.formula(x, code, ...)
}

#' @export
repair_attributes.formula <- function(x, code, ..., pipe ="base") {
  opts <- fetch_opts("formula", ...)
  constructor <- opts$constructor
  ignore <- ".Environment"
  if (constructor == "~") {
    if (opts$environment) {
      ignore <- NULL # don't ignore the .Environment attr, we need to set it up
      code <- wrap(code, "")
    } else {
      additional_args <- setdiff(rlang::names2(attributes(x)), c(".Environment", "class"))
      if (length(additional_args) || !identical(class(x), "formula")) {
        code <- wrap(code, "")
      }
    }
  }
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    idiomatic_class = "formula",
    ignore = ignore
  )
}

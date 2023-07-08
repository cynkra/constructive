constructors$formula <- new.env()

#' Constructive options for formulas
#'
#' These options will be used on formulas, defined as calls to `~`, regardless
#' of their `"class"` attribute.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @param environment Boolean. Whether to attempt to construct the environment, if
#'   it makes a difference to construct it.
#'
#' Depending on `constructor`, we construct the formula as follows:
#' * `"~"` (default): We construct the formula in the most common way using the `~`
#'   operator.
#' * `"formula"` : deparse the formula as a string and use `base::formula()` on top of it.
#' * `"as.formula"` : Same as above, but using `base::as.formula()`.
#' * `"new_formula"` : extract both sides of the formula as separate language objects
#'   and feed them to `rlang::new_formula()`, along with the reconstructed environment
#'   if relevant.
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_formula <- function(constructor = c("~", "formula", "as.formula", "new_formula"), ..., environment = TRUE) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "formula"),
    ellipsis::check_dots_empty(),
    abort_not_boolean(environment)
  )
  .cstr_options("formula", constructor = constructor, environment = environment)
}

#' @export
.cstr_construct.formula <- function(x, ..., env) {
  opts <- .cstr_fetch_opts("formula", ...)
  if (is_corrupted_formula(x)) return(NextMethod())
  constructor <- constructors$formula[[opts$constructor]]
  env_is_default <- identical(attr(x, ".Environment"), env)
  constructor(x, ..., environment = opts$environment, env = env, env_is_default = env_is_default)
}

is_corrupted_formula <- function(x) {
  !is.call(x) || !identical(.subset2(x, 1), quote(`~`))
}

constructors$formula$"~" <- function(x, ..., environment, env_is_default) {
  code <- deparse(x)
  repair_attributes_formula(x, code, ..., ignore_env_attr = env_is_default || !environment)
}


constructors$formula$new_formula <- function(x, ..., environment, env_is_default) {
  lhs_code <- .cstr_construct(rlang::f_lhs(x), ...)
  rhs_code <- .cstr_construct(rlang::f_rhs(x), ...)
  if (environment && !env_is_default) {
    env_code <- .cstr_construct(environment(x), ...)
    code <- .cstr_apply(list(lhs_code, rhs_code, env = env_code), "rlang::new_formula", ..., recurse = FALSE)
  } else {
    code <- .cstr_apply(list(lhs_code, rhs_code), "rlang::new_formula", ..., recurse = FALSE)
  }
  repair_attributes_formula(x, code, ...)
}

constructors$formula$formula <- function(x, ..., environment, env_is_default) {
  if (environment && !env_is_default) {
    code <- .cstr_apply(list(deparse(x), env = environment(x)), "formula", ...)
  } else {
    code <- .cstr_apply(list(deparse(x)), "formula", ..., recurse = FALSE)
  }
  repair_attributes_formula(x, code, ...)
}

constructors$formula$as.formula <- function(x, ..., environment, env_is_default) {
  if (environment && !env_is_default) {
    code <- .cstr_apply(list(deparse(x), env = environment(x)), "as.formula", ...)
  } else {
    code <- .cstr_apply(list(deparse(x)), "as.formula", ..., recurse = FALSE)
  }
  repair_attributes_formula(x, code, ...)
}

repair_attributes_formula <- function(x, code, ..., pipe = NULL, ignore_env_attr = TRUE) {
  opts <- .cstr_fetch_opts("formula", ...)
  ignore <- NULL

  if (ignore_env_attr) {
    ignore <- ".Environment"
    some_attrs_were_not_idiomatically_constructed <-
      !all(rlang::names2(attributes(x)) %in% c(".Environment", "class")) ||
      !identical(class(x), "formula")
    if (some_attrs_were_not_idiomatically_constructed) {
      code <- .cstr_wrap(code, "")
    }
  } else {
    code <- .cstr_wrap(code, "")
  }

  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    idiomatic_class = "formula",
    ignore = ignore
  )
}

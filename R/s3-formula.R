#' Constructive options for formulas
#'
#' These options will be used on formulas, defined as calls to `~`, regardless
#' of their `"class"` attribute.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @param environment Boolean. Whether to attempt to construct the environment, if
#'   it makes a difference to construct it.
#'
#' Depending on `constructor`, we construct the formula as follows:
#' * `"default"`: We construct the formula in the most common way using the `~`
#'   operator.
#' * `"formula"` : deparse the formula as a string and use `base::formula()` on top of it.
#' * `"as.formula"` : Same as above, but using `base::as.formula()`.
#' * `"new_formula"` : extract both sides of the formula as separate language objects
#'   and feed them to `rlang::new_formula()`, along with the reconstructed environment
#'   if relevant.
#'
#' @return An object of class <constructive_options/constructive_options_formula>
#' @export
opts_formula <- function(constructor = c("default", "formula", "as.formula", "new_formula", "next"), ..., environment = TRUE) {
  .cstr_options("formula", constructor = constructor[[1]], ..., environment = environment)
}

#' @export
#' @method .cstr_construct formula
.cstr_construct.formula <- function(x, ..., env = parent.frame()) {
  opts <- list(...)$opts$formula %||% opts_formula()
  if (is_corrupted_formula(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.formula", structure(NA, class = opts$constructor))
}

is_corrupted_formula <- function(x) {
  !is.call(x) || !identical(.subset2(x, 1), quote(`~`))
}

#' @export
#' @method .cstr_construct.formula default
.cstr_construct.formula.default <- function(x, ..., env = parent.frame()) {
  opts <- list(...)$opts$formula %||% opts_formula()
  env_is_default <- identical(attr(x, ".Environment"), env)
  code <- deparse(x)
  repair_attributes_formula(x, code, ..., ignore_env_attr = env_is_default || !opts$environment)
}

#' @export
#' @method .cstr_construct.formula new_formula
.cstr_construct.formula.new_formula <- function(x, ..., env) {
  opts <- list(...)$opts$formula %||% opts_formula()
  env_is_default <- identical(attr(x, ".Environment"), env)
  lhs_code <- .cstr_construct(rlang::f_lhs(x), ...)
  rhs_code <- .cstr_construct(rlang::f_rhs(x), ...)
  if (opts$environment && !env_is_default) {
    env_code <- .cstr_construct(environment(x), ...)
    code <- .cstr_apply(list(lhs_code, rhs_code, env = env_code), "rlang::new_formula", ..., recurse = FALSE)
  } else {
    code <- .cstr_apply(list(lhs_code, rhs_code), "rlang::new_formula", ..., recurse = FALSE)
  }
  repair_attributes_formula(x, code, ...)
}

#' @export
#' @method .cstr_construct.formula formula
.cstr_construct.formula.formula <- function(x, ..., env) {
  opts <- list(...)$opts$formula %||% opts_formula()
  env_is_default <- identical(attr(x, ".Environment"), env)
  if (opts$environment && !env_is_default) {
    code <- .cstr_apply(list(deparse(x), env = environment(x)), "formula", ...)
  } else {
    code <- .cstr_apply(list(deparse(x)), "formula", ..., recurse = FALSE)
  }
  repair_attributes_formula(x, code, ...)
}

#' @export
#' @method .cstr_construct.formula as.formula
.cstr_construct.formula.as.formula <- function(x, ..., env) {
  opts <- list(...)$opts$formula %||% opts_formula()
  env_is_default <- identical(attr(x, ".Environment"), env)
  if (opts$environment && !env_is_default) {
    code <- .cstr_apply(list(deparse(x), env = environment(x)), "as.formula", ...)
  } else {
    code <- .cstr_apply(list(deparse(x)), "as.formula", ..., recurse = FALSE, env = env)
  }
  repair_attributes_formula(x, code, ..., env = env)
}

repair_attributes_formula <- function(x, code, ..., pipe = NULL, ignore_env_attr = TRUE) {
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

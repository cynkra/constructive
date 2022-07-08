#' @export
construct_idiomatic.formula <- function(x, ...) {
  env <- environment(x)
  x_chr <- deparse(x)
  if (is.null(env)) return(deparse(x_chr))
  env_chr <- construct_raw(env, ...)
  construct_apply(list(x_chr, env_chr), "rlang::set_env", language = TRUE, ...)
}

#' @export
repair_attributes.formula <- function(x, code, pipe ="base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    idiomatic_class = "formula",
    ignore = ".Environment",
    ...
  )
}

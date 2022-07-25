#' @export
construct_idiomatic.formula <- function(x, ...) {
  env <- environment(x)
  x_chr <- deparse(x)
  env_chr <- construct_raw(env, ...)
  construct_apply(list(x_chr, env_chr), "match.fun(\"environment<-\")", language = TRUE, ...)
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

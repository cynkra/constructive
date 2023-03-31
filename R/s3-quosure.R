#' @export
construct_idiomatic.quosure <- function(x, ...) {
  quo_code <- deparse_call_impl(rlang::quo_squash(x))
  quo_code[[1]] <- paste0("~", quo_code[[1]])
  env_code <- construct_raw(attr(x, ".Environment"), ...)
  construct_apply(list(quo_code, env_code), "rlang::as_quosure", language = TRUE, ...)
}

#' @export
repair_attributes.quosure <- function(x, code, ...) {
  repair_attributes_impl(
    x, code, ...,
    ignore = ".Environment",
    idiomatic_class = c("quosure", "formula")
  )
}

#' @export
construct_idiomatic.quosures <- function(x, ...) {
  x_list <- unclass(x)
  # remove names if "" so we avoid repairing the names
  if (all(names(x) == "")) names(x_list) <- NULL
  list_code <- construct_raw(x_list, ...)
  construct_apply(list(list_code), "rlang::as_quosures", language = TRUE, ...)
}

#' @export
repair_attributes.quosures <- function(x, code, ...) {
  repair_attributes_impl(
    x, code, ...,
    idiomatic_class = c("quosures", "list"),
    ignore = if (all(names(x) == "")) "names"
  )
}

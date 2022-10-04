#' @export
construct_idiomatic.Date <- function(x, ...) {
  if (any(is.infinite(x))) {
    construct_apply(list(unclass(x), origin = "1970-01-01"), "as.Date", ..., new_line = FALSE)
  } else {
    construct_apply(list(format(x)), "as.Date", ..., new_line = FALSE)
  }
}

#' @export
repair_attributes.Date <- function(x, code, ..., pipe = "base") {
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    idiomatic_class = "Date"
  )
}

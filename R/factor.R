#' @export
construct_idiomatic.factor <- function(x, ...) {
  default_levs <- sort(unique(as.character(x)))
  levs <- levels(x)
  args <- if (identical(default_levs, levs)) {
    construct_apply(list(levs[x]), "factor", new_line =  FALSE, ...)
  } else {
    construct_apply(list(levs[x], levels = levs), "factor", ...)
  }
}

#' @export
repair_attributes.factor <- function(x, code, pipe = "base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    ignore = "levels",
    idiomatic_class = "factor",
    ...
  )
}

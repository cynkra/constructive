#' @export
construct_idiomatic.ordered <- function(x, ...) {
  default_levs <- sort(unique(as.character(x)))
  levs <- levels(x)
  args <- if (identical(default_levs, levs)) list(levs[x]) else list(levs[x], levels = levs)
  construct_apply(args, "ordered", ...)
}

#' @export
repair_attributes.ordered <- function(x, code, pipe = "base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    ignore = "levels",
    idiomatic_class = c("ordered", "factor"),
    ...
  )
}

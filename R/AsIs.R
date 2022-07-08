#' @export
construct_idiomatic.AsIs <- function(x, ...) {
  class(x) <- setdiff(oldClass(x), "AsIs")
  wrap(construct_raw(x, ...), "I", new_line = FALSE)
}

#' @export
repair_attributes.AsIs <- function(x, code, ...) {
  # no reparation needed, this will be dealt with in `I()`'s arg
  code
}

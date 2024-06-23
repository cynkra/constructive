#' @export
#' @rdname other-opts
opts_rel <- function(constructor = c("rel", "next", "double"), ...) {
  .cstr_options("rel", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct rel
.cstr_construct.rel <- function(x, ...) {
  opts <- list(...)$opts$rel %||% opts_rel()
  if (is_corrupted_rel(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.rel", structure(NA, class = opts$constructor))
}

is_corrupted_rel <- function(x) {
  # rel() really just sets a class so no object is really corrupted in the sense
  # that "rel" objects can all be recreated, but it does expect a numeric input
  # to work with other ggplot functions
  FALSE
}

#' @export
#' @method .cstr_construct.rel double
.cstr_construct.rel.double <- function(x, ...) {
  if (!is.double(x)) return(.cstr_construct.default(x, ...))
  .cstr_construct.double(x, ...)
}

#' @export
#' @method .cstr_construct.rel rel
.cstr_construct.rel.rel <- function(x, ...) {
  code <- .cstr_apply(list(unclass(x)), "ggplot2::rel", ...)
  repair_attributes_rel(x, code, ...)
}

repair_attributes_rel <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = "rel", ...)
}

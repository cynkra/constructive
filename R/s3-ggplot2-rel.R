#' @export
#' @rdname other-opts
opts_rel <- function(constructor = c("rel", "next", "atomic"), ...) {
  .cstr_options("rel", constructor = constructor[[1]], ...)
}

#' @export
.cstr_construct.rel <- function(x, ...) {
  opts <- list(...)$opts$rel %||% opts_rel()
  if (is_corrupted_rel(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.rel", structure(NA, class = opts$constructor))
}

is_corrupted_rel <- function(x) {
  # TODO
  FALSE
}

#' @export
.cstr_construct.rel.atomic <- function(x, ...) {
  .cstr_construct.atomic(x, ...)
}

#' @export
.cstr_construct.rel.rel <- function(x, ...) {
  code <- .cstr_apply(list(unclass(x)), "ggplot2::rel", ...)
  repair_attributes_rel(x, code, ...)
}

repair_attributes_rel <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = "rel", ...)
}

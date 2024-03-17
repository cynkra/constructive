constructors$rel <- new.env()

#' @export
#' @rdname other-opts
opts_rel <- new_constructive_opts_function("rel", c("rel", "next", "atomic"))

#' @export
.cstr_construct.rel <- new_constructive_method("rel", c("rel", "next", "atomic"))

is_corrupted_rel <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$rel$atomic <- function(x, ...) {
  .cstr_construct.atomic(x, ...)
}

#' @export
constructors$rel$rel <- function(x, ...) {
  code <- .cstr_apply(list(unclass(x)), "ggplot2::rel", ...)
  repair_attributes_rel(x, code, ...)
}

repair_attributes_rel <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = "rel", ...)
}

constructors$labels <- new.env()

#' @export
#' @rdname other-opts
opts_labels <- new_constructive_opts_function("labels", c("labs", "next", "list"))

#' @export
.cstr_construct.labels <- new_constructive_method("labels", c("labs", "next", "list"))

is_corrupted_labels <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$labels$list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
constructors$labels$labs <- function(x, ...) {
  code <- .cstr_apply(x, fun = "ggplot2::labs", ...)
  repair_attributes_labels(x, code, ...)
}

repair_attributes_labels <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = "labels", ...)
}

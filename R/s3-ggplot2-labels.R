#' @export
#' @rdname other-opts
opts_labels <- function(constructor = c("labs", "next", "list"), ...) {
  .cstr_options("labels", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct labels
.cstr_construct.labels <- function(x, ...) {
  opts <- list(...)$opts$labels %||% opts_labels()
  if (is_corrupted_labels(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.labels", structure(NA, class = opts$constructor))
}

is_corrupted_labels <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.labels list
.cstr_construct.labels.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
#' @method .cstr_construct.labels labs
.cstr_construct.labels.labs <- function(x, ...) {
  code <- .cstr_apply(x, fun = "ggplot2::labs", ...)
  repair_attributes_labels(x, code, ...)
}

repair_attributes_labels <- function(x, ...) {
  if (with_versions(ggplot2 <= "3.5.2")) {
    idiomatic_class <- "labels"
  } else {
    idiomatic_class <- c("labels", "gg")
  }
  .cstr_repair_attributes(x, idiomatic_class = idiomatic_class, ...)
}

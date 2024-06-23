#' @export
#' @rdname other-opts
opts_element_grob <- function(constructor = c("element_grob", "next", "list"), ...) {
  .cstr_options("element_grob", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct element_grob
.cstr_construct.element_grob <- function(x, ...) {
  opts <- list(...)$opts$element_grob %||% opts_element_grob()
  if (is_corrupted_element_grob(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.element_grob", structure(NA, class = opts$constructor))
}

is_corrupted_element_grob <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.element_grob list
.cstr_construct.element_grob.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
#' @method .cstr_construct.element_grob element_grob
.cstr_construct.element_grob.element_grob <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_grob)
  code <- .cstr_apply(args, "ggplot2::element_grob", ...)
  repair_attributes_element_grob(x, code, ...)
}

repair_attributes_element_grob <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_grob", "element"), ...)
}

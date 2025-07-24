#' @export
#' @rdname other-opts
opts_element_rect <- function(constructor = c("element_rect", "next", "list"), ...) {
  .cstr_options("element_rect", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct element_rect
.cstr_construct.element_rect <- function(x, ...) {
  opts <- list(...)$opts$element_rect %||% opts_element_rect()
  if (is_corrupted_element_rect(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.element_rect", structure(NA, class = opts$constructor))
}

is_corrupted_element_rect <- function(x) {
  !is.list(x)
}

#' @export
#' @method .cstr_construct.element_rect list
.cstr_construct.element_rect.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
#' @method .cstr_construct.element_rect element_rect
.cstr_construct.element_rect.element_rect <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_rect)
  code <- .cstr_apply(args, "ggplot2::element_rect", ...)
  repair_attributes_element_rect(x, code, ...)
}

repair_attributes_element_rect <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_rect", "element"), ...)
}

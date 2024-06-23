#' @export
#' @rdname other-opts
opts_element_blank <- function(constructor = c("element_blank", "next", "list"), ...) {
  .cstr_options("element_blank", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct element_blank
.cstr_construct.element_blank <- function(x, ...) {
  opts <- list(...)$opts$element_blank %||% opts_element_blank()
  if (is_corrupted_element_blank(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.element_blank", structure(NA, class = opts$constructor))
}

is_corrupted_element_blank <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.element_blank list
.cstr_construct.element_blank.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
#' @method .cstr_construct.element_blank element_blank
.cstr_construct.element_blank.element_blank <- function(x, ...) {
  code <- "ggplot2::element_blank()"
  repair_attributes_element_blank(x, code, ...)
}

repair_attributes_element_blank <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_blank", "element"), ...)
}

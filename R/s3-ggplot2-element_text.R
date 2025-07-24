#' @export
#' @rdname other-opts
opts_element_text <- function(constructor = c("element_text", "next", "list"), ...) {
  .cstr_options("element_text", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct element_text
.cstr_construct.element_text <- function(x, ...) {
  opts <- list(...)$opts$element_text %||% opts_element_text()
  if (is_corrupted_element_text(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.element_text", structure(NA, class = opts$constructor))
}

is_corrupted_element_text <- function(x) {
  !is.list(x)
}

#' @export
#' @method .cstr_construct.element_text list
.cstr_construct.element_text.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
#' @method .cstr_construct.element_text element_text
.cstr_construct.element_text.element_text <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_text)
  code <- .cstr_apply(args, "ggplot2::element_text", ...)
  repair_attributes_element_text(x, code, ...)
}

repair_attributes_element_text <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_text", "element"), ...)
}

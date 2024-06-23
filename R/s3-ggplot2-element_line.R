#' @export
#' @rdname other-opts
opts_element_line <- function(constructor = c("element_line", "next", "list"), ...) {
  .cstr_options("element_line", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct element_line
.cstr_construct.element_line <- function(x, ...) {
  opts <- list(...)$opts$element_line %||% opts_element_line()
  if (is_corrupted_element_line(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.element_line", structure(NA, class = opts$constructor))
}

is_corrupted_element_line <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.element_line list
.cstr_construct.element_line.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
#' @method .cstr_construct.element_line element_line
.cstr_construct.element_line.element_line <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_line)
  code <- .cstr_apply(args, "ggplot2::element_line", ...)
  repair_attributes_element_line(x, code, ...)
}

repair_attributes_element_line <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_line", "element"), ...)
}

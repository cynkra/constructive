#' @export
#' @rdname other-opts
opts_element_render <- function(constructor = c("element_render", "next", "list"), ...) {
  .cstr_options("element_render", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct element_render
.cstr_construct.element_render <- function(x, ...) {
  opts <- list(...)$opts$element_render %||% opts_element_render()
  if (is_corrupted_element_render(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.element_render", structure(NA, class = opts$constructor))
}

is_corrupted_element_render <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.element_render list
.cstr_construct.element_render.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
#' @method .cstr_construct.element_render element_render
.cstr_construct.element_render.element_render <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_render)
  code <- .cstr_apply(args, "ggplot2::element_render", ...)
  repair_attributes_element_render(x, code, ...)
}

repair_attributes_element_render <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_render", "element"), ...)
}

#' @export
#' @rdname other-opts
opts_ggplot2_element_blank <- function(constructor = c("element_blank", "next"), ...) {
  .cstr_options("element_blank", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ggplot2::element_blank
`.cstr_construct.ggplot2::element_blank` <- function(x, ...) {
  opts <- list(...)$opts$element_blank %||% opts_ggplot2_element_blank()
  if (`is_corrupted_ggplot2::element_blank`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ggplot2::element_blank", structure(NA, class = opts$constructor))
}

`is_corrupted_ggplot2::element_blank` <- function(x) {
  typeof(x) != "object"
}

#' @export
#' @method .cstr_construct.ggplot2::element_blank element_blank
`.cstr_construct.ggplot2::element_blank.element_blank` <- function(x, ...) {
  code <- "ggplot2::element_blank()"
  `repair_attributes_ggplot2::element_blank`(x, code, ...)
}

`repair_attributes_ggplot2::element_blank` <- function(x, ...) {
  property_names <- names(attr(attr(x, "S7_class"),"properties"))
  .cstr_repair_attributes(
    x,
    idiomatic_class = c(
      "ggplot2::element_blank",
      "element_blank",
      "ggplot2::element",
      "S7_object",
      "element"
    ),
    ignore = "S7_class",
    ...
  )
}

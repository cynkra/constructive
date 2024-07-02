#' @importFrom constructive .cstr_construct .cstr_apply
NULL

#' @export
#' @method .cstr_construct.CLASS1 CONSTRUCTOR
.cstr_construct.CLASS1.CONSTRUCTOR <- function(x, ...) {
  # opts <- list(...)$opts$CLASS1 %||% opts_CLASS1()
  args <- list()
  code <- .cstr_apply(args, fun = "CONSTRUCTOR", ...)
  repair_attributes_CLASS1(x, code, ...)
}

#' @export
#' @method .cstr_construct.CLASS CONSTRUCTOR
.cstr_construct.CLASS.CONSTRUCTOR <- function(x, ...) {
  opts <- list(...)$opts$CLASS %||% opts_CLASS()
  args <- ...
  code <- .cstr_apply(args, fun = "CONSTRUCTOR", ...)
  repair_attributes_CLASS(x, code, ...)
}

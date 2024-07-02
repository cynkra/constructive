#' @importFrom constructive .cstr_construct .cstr_apply
NULL

#' @export
.cstr_construct..CLASS1...CONSTRUCTOR. <- function(x, ...) {
  # opts <- list(...)$opts$.CLASS1. %||% opts_.CLASS1.()
  args <- list()
  code <- .cstr_apply(args, fun = ".PKG::CONSTRUCTOR.", ...)
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = .CLASS.
  )
}

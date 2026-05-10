#' @exportS3Method constructive::.cstr_construct..CLASS1.
.cstr_construct..CLASS1...CONSTRUCTOR. <- function(x, ...) {
  # opts <- list(...)$opts$.CLASS1. %||% opts_.CLASS1.()
  args <- list()
  code <- constructive::.cstr_apply(args, fun = ".PKG::CONSTRUCTOR.", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = .CLASS.
  )
}

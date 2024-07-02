#' @export
#' @method .cstr_construct.CLASS CONSTRUCTOR
.cstr_construct.CLASS.CONSTRUCTOR <- function(x, ...) {
  # can be removed if your constructor doesn't use additional options
  opts <- list(...)$opts$CLASS %||% opts_CLASS()
  args <- ...
  # This creates a call CONSTRUCTOR(...) where ... is the constructed code
  # of the arguments stored in `args`
  # Sometimes we need to construct the code of the args separately, and
  # use `recurse = FALSE` below, see `?.cstr_apply`
  code <- .cstr_apply(args, fun = "CONSTRUCTOR", ...)
  # attribute reparation is strictly speaking constructor specific but in
  # most cases it's the same within a class
  repair_attributes_CLASS(x, code, ...)
}

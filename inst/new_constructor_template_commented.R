#' @export
#' @method .cstr_construct.CLASS1 CONSTRUCTOR
.cstr_construct.CLASS1.CONSTRUCTOR <- function(x, ...) {
  # Uncomment if your constructor needs additional options from opts_CLASS1()
  # opts <- list(...)$opts$CLASS1 %||% opts_CLASS1()

  # Instead of the call below we need to fetch the args of the constructor in `x`.
  args <- list()

  # This creates a call CONSTRUCTOR(...) where ... is the constructed code
  # of the arguments stored in `args`
  # Sometimes we want to construct the code of the args separately, i.e. store
  # code rather than objects in `args`, and use `recurse = FALSE` below
  code <- .cstr_apply(args, fun = "CONSTRUCTOR", ...)

  # attribute reparation is strictly speaking constructor specific but in
  # most cases it's the same within a class
  repair_attributes_CLASS1(x, code, ...)
}

#' @importFrom constructive .cstr_construct .cstr_apply
NULL

#' @export
.cstr_construct..CLASS1...CONSTRUCTOR. <- function(x, ...) {
  # Uncomment if your constructor needs additional options from opts_.CLASS1.()
  # opts <- list(...)$opts$.CLASS1. %||% opts_.CLASS1.()

  # Instead of the call below we need to fetch the args of the constructor in `x`.
  args <- list()

  # This creates a call .CONSTRUCTOR.(...) where ... is the constructed code
  # of the arguments stored in `args`
  # Sometimes we want to construct the code of the args separately, i.e. store
  # code rather than objects in `args`, and use `recurse = FALSE` below
  code <- .cstr_apply(args, fun = ".PKG::CONSTRUCTOR.", ...)

  # .cstr_repair_attributes() makes sure that attributes that are not built
  # by the idiomatic constructor are generated
  .cstr_repair_attributes(
    x, code, ...,
    # attributes built by the constructor
    # ignore =,

    # not necessarily just a string, but the whole class(x) vector
    idiomatic_class = .CLASS.
  )
}

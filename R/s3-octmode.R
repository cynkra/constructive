#' @export
#' @rdname other-opts
opts_octmode <- function(constructor = c("as.octmode", "next"), ..., integer = FALSE) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    abort_not_boolean(integer),
    check_dots_empty()
  )
  .cstr_options("octmode", constructor = constructor, integer = integer)
}

#' @export
.cstr_construct.octmode <- function(x, opts, ...) {
  opts_local <- opts$octmode %||% opts_octmode()
  if (is_corrupted_octmode(x) || opts_local[["constructor"]] == "next") return(NextMethod())
  constructor <- constructors$octmode[[opts_local[["constructor"]]]]
  constructor(x, opts = opts, ..., integer = opts_local$integer)
}

is_corrupted_octmode <- function(x) {
  !(is.integer(x))
}

#' @export
constructors$octmode$as.octmode <- function(x, ..., integer) {
  # we let attributes to the reparation step
  x_bkp <- x
  attributes(x) <- NULL
  if (integer) {
    code <- .cstr_apply(list(unclass(x)), "as.octmode", ...)
  } else {
    code <- .cstr_apply(list(format.octmode(x)), "as.octmode", ...)
  }
  repair_attributes_octmode(x_bkp, code, ...)
}

repair_attributes_octmode <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe,
    idiomatic_class = "octmode",
    ...
  )
}

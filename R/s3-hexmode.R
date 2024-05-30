#' @export
#' @rdname other-opts
#' @param integer Whether to use `as.hexmode()` on integer rather than character
opts_hexmode <- function(constructor = c("as.hexmode", "next"), ..., integer = FALSE) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    abort_not_boolean(integer),
    check_dots_empty()
  )
  .cstr_options("hexmode", constructor = constructor, integer = integer)
}

#' @export
.cstr_construct.hexmode <- function(x, opts = NULL, ...) {
  opts_local <- opts$hexmode %||% opts_hexmode()
  if (is_corrupted_hexmode(x) || opts_local[["constructor"]] == "next") return(NextMethod())
  constructor <- constructors$hexmode[[opts_local[["constructor"]]]]
  constructor(x, opts = opts, ..., integer = opts_local[["integer"]])
}

is_corrupted_hexmode <- function(x) {
  !(is.integer(x))
}

#' @export
constructors$hexmode$as.hexmode <- function(x, ..., integer) {
  # we let attributes to the reparation step
  x_bkp <- x
  attributes(x) <- NULL
  if (integer) {
    code <- .cstr_apply(list(unclass(x)), "as.hexmode", ...)
  } else {
    code <- .cstr_apply(list(format.hexmode(x)), "as.hexmode", ...)
  }
  repair_attributes_hexmode(x_bkp, code, ...)
}

repair_attributes_hexmode <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe,
    idiomatic_class = "hexmode",
    ...
  )
}

#' @export
#' @rdname other-opts
#' @param integer Whether to use `as.hexmode()` on integer rather than character
opts_hexmode <- function(constructor = c("as.hexmode", "next"), ..., integer = FALSE) {
  abort_not_boolean(integer)
  .cstr_options("hexmode", constructor = constructor[[1]], ..., integer = integer)
}

#' @export
#' @method .cstr_construct hexmode
.cstr_construct.hexmode <- function(x, ...) {
  opts <- list(...)$opts$hexmode %||% opts_hexmode()
  if (is_corrupted_hexmode(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.hexmode", structure(NA, class = opts$constructor))
}

is_corrupted_hexmode <- function(x) {
  !(is.integer(x))
}

#' @export
#' @method .cstr_construct.hexmode as.hexmode
.cstr_construct.hexmode.as.hexmode <- function(x, ...) {
  opts <- list(...)$opts$hexmode %||% opts_hexmode()
  # we let attributes to the reparation step
  x_bkp <- x
  attributes(x) <- NULL
  if (opts$integer) {
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

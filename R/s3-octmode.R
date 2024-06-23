#' @export
#' @rdname other-opts
opts_octmode <- function(constructor = c("as.octmode", "next"), ..., integer = FALSE) {
  abort_not_boolean(integer)
  .cstr_options("octmode", constructor = constructor[[1]], ..., integer = integer)
}

#' @export
.cstr_construct.octmode <- function(x, ...) {
  opts <- list(...)$opts$octmode %||% opts_octmode()
  if (is_corrupted_octmode(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.octmode", structure(NA, class = opts$constructor))
}

is_corrupted_octmode <- function(x) {
  !(is.integer(x))
}

#' @export
.cstr_construct.octmode.as.octmode <- function(x, ...) {
  opts <- list(...)$opts$octmode %||% opts_octmode()
  # we let attributes to the reparation step
  x_bkp <- x
  attributes(x) <- NULL
  if (opts$integer) {
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

#' @export
#' @rdname other-opts
opts_simpleError <- function(constructor = c("simpleError", "next"), ...) {
  .cstr_options("simpleError", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct simpleError
.cstr_construct.simpleError <- function(x, ...) {
  opts <- list(...)$opts$simpleError %||% opts_simpleError()
  if (is_corrupted_simpleError(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.simpleError", structure(NA, class = opts$constructor))
}

is_corrupted_simpleError <- function(x) {
  !(is.list(x) && identical(names(x)[1:2], c("message", "call")))
}

#' @export
#' @method .cstr_construct.simpleError simpleError
.cstr_construct.simpleError.simpleError <- function(x, ...) {
  # we let attributes to the repair step
  x_bkp <- x
  x <- unclass(x)
  if (is.null(x$call)) {
    code <- .cstr_apply(list(x$message), "simpleError", ...)
  } else {
    code <- .cstr_apply(list(x$message, call = x$call), "simpleError", ...)
  }
  repair_attributes_simpleError(x_bkp, code, ...)
}

repair_attributes_simpleError <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe = pipe,
    idiomatic_class = c("simpleError", "error", "condition"),
    ...
  )
}

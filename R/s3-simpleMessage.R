#' @export
#' @rdname other-opts
opts_simpleMessage <- function(constructor = c("simpleMessage", "next"), ...) {
  .cstr_options("simpleMessage", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct simpleMessage
.cstr_construct.simpleMessage <- function(x, ...) {
  opts <- list(...)$opts$simpleMessage %||% opts_simpleMessage()
  if (is_corrupted_simpleMessage(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.simpleMessage", structure(NA, class = opts$constructor))
}

is_corrupted_simpleMessage <- function(x) {
  !(is.list(x) && identical(names(x)[1:2], c("message", "call")))
}

#' @export
#' @method .cstr_construct.simpleMessage simpleMessage
.cstr_construct.simpleMessage.simpleMessage <- function(x, ...) {
  # we let attributes to the repair step
  x_bkp <- x
  x <- unclass(x)
  if (is.null(x$call)) {
    code <- .cstr_apply(list(x$message), "simpleMessage", ...)
  } else {
    code <- .cstr_apply(list(x$message, call = x$call), "simpleMessage", ...)
  }
  repair_attributes_simpleMessage(x_bkp, code, ...)
}

repair_attributes_simpleMessage <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe,
    idiomatic_class = c("simpleMessage", "message", "condition"),
    ...
  )
}

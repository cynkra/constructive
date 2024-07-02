#' @export
#' @rdname other-opts
opts_simpleWarning <- function(constructor = c("simpleWarning", "next"), ...) {
  .cstr_options("simpleWarning", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct simpleWarning
.cstr_construct.simpleWarning <- function(x, ...) {
  opts <- list(...)$opts$simpleWarning %||% opts_simpleWarning()
  if (is_corrupted_simpleWarning(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.simpleWarning", structure(NA, class = opts$constructor))
}

is_corrupted_simpleWarning <- function(x) {
  !(is.list(x) && identical(names(x)[1:2], c("message", "call")))
}

#' @export
#' @method .cstr_construct.simpleWarning simpleWarning
.cstr_construct.simpleWarning.simpleWarning <- function(x, ...) {
  # we let attributes to the repair step
  x_bkp <- x
  x <- unclass(x)
  if (is.null(x$call)) {
    code <- .cstr_apply(list(x$message), "simpleWarning", ...)
  } else {
    code <- .cstr_apply(list(x$message, call = x$call), "simpleWarning", ...)
  }
  repair_attributes_simpleWarning(x_bkp, code, ...)
}

repair_attributes_simpleWarning <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe,
    idiomatic_class = c("simpleWarning", "warning", "condition"),
    ...
  )
}

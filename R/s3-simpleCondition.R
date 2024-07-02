#' @export
#' @rdname other-opts
opts_simpleCondition <- function(constructor = c("simpleCondition", "next"), ...) {
  .cstr_options("simpleCondition", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct simpleCondition
.cstr_construct.simpleCondition <- function(x, ...) {
  opts <- list(...)$opts$simpleCondition %||% opts_simpleCondition()
  if (is_corrupted_simpleCondition(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.simpleCondition", structure(NA, class = opts$constructor))
}

is_corrupted_simpleCondition <- function(x) {
  !(is.list(x) && identical(names(x)[1:2], c("message", "call")))
}

#' @export
#' @method .cstr_construct.simpleCondition simpleCondition
.cstr_construct.simpleCondition.simpleCondition <- function(x, ...) {
  # we let attributes to the repair step
  x_bkp <- x
  x <- unclass(x)
  if (is.null(x$call)) {
    code <- .cstr_apply(list(x$message), "simpleCondition", ...)
  } else {
    code <- .cstr_apply(list(x$message, call = x$call), "simpleCondition", ...)
  }
  repair_attributes_simpleCondition(x_bkp, code, ...)
}

repair_attributes_simpleCondition <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe,
    idiomatic_class = c("simpleCondition", "condition"),
    ...
  )
}

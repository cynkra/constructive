#' @export
#' @rdname other-opts
opts_simpleError <- function(constructor = c("simpleError", "next"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("simpleError", constructor = constructor)
}

#' @export
.cstr_construct.simpleError <- function(x, opts, ...) {
  opts_local <- opts$simpleError %||% opts_simpleError()
  if (is_corrupted_simpleError(x) || opts_local[["constructor"]] == "next") return(NextMethod())
  constructor <- constructors$simpleError[[opts_local[["constructor"]]]]
  constructor(x, opts = opts, ...)
}

is_corrupted_simpleError <- function(x) {
  !(is.list(x) && identical(names(x)[1:2], c("message", "call")))
}

#' @export
constructors$simpleError$simpleError <- function(x, ...) {
  # we let attributes to the reparation step
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
    x, code, pipe,
    idiomatic_class = c("simpleError", "error", "condition"),
    ...
  )
}

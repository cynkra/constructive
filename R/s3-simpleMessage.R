#' @export
#' @rdname other-opts
opts_simpleMessage <- function(constructor = c("simpleMessage", "next"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("simpleMessage", constructor = constructor)
}

#' @export
.cstr_construct.simpleMessage <- function(x, opts, ...) {
  opts_local <- opts$simpleMessage %||% opts_simpleMessage()
  if (is_corrupted_simpleMessage(x) || opts_local$constructor == "next") return(NextMethod())
  constructor <- constructors$simpleMessage[[opts_local$constructor]]
  constructor(x, opts = opts, ...)
}

is_corrupted_simpleMessage <- function(x) {
  !(is.list(x) && identical(names(x)[1:2], c("message", "call")))
}

#' @export
constructors$simpleMessage$simpleMessage <- function(x, ...) {
  # we let attributes to the reparation step
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

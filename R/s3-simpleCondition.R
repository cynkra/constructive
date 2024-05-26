#' @export
#' @rdname other-opts
opts_simpleCondition <- function(constructor = c("simpleCondition", "next"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("simpleCondition", constructor = constructor)
}

#' @export
.cstr_construct.simpleCondition <- function(x, opts, ...) {
  opts_local <- opts$simpleCondition %||% opts_simpleCondition()
  if (is_corrupted_simpleCondition(x) || opts_local$constructor == "next") return(NextMethod())
  constructor <- constructors$simpleCondition[[opts_local$constructor]]
  constructor(x, opts = opts, ...)
}

is_corrupted_simpleCondition <- function(x) {
  !(is.list(x) && identical(names(x)[1:2], c("message", "call")))
}

#' @export
constructors$simpleCondition$simpleCondition <- function(x, ...) {
  # we let attributes to the reparation step
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

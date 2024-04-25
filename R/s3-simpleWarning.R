#' @export
#' @rdname other-opts
opts_simpleWarning <- function(constructor = c("simpleWarning", "next"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("simpleWarning", constructor = constructor)
}

#' @export
.cstr_construct.simpleWarning <- function(x, ...) {
  opts <- .cstr_fetch_opts("simpleWarning", ...)
  if (is_corrupted_simpleWarning(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$simpleWarning[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_simpleWarning <- function(x) {
  !(is.list(x) && identical(names(x)[1:2], c("message", "call")))
}

#' @export
constructors$simpleWarning$simpleWarning <- function(x, ...) {
  # we let attributes to the reparation step
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

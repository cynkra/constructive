#' @export
#' @rdname other-opts
opts_error <- function(constructor = c("errorCondition", "next"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("error", constructor = constructor)
}

#' @export
.cstr_construct.error <- function(x, opts, ...) {
  opts_local <- opts$error %||% opts_error()
  if (is_corrupted_error(x) || opts_local[["constructor"]] == "next") return(NextMethod())
  constructor <- constructors$error[[opts_local[["constructor"]]]]
  constructor(x, opts = opts, ...)
}

is_corrupted_error <- function(x) {
  !(is.list(x) && identical(names(x)[1:2], c("message", "call")))
}

#' @export
constructors$error$errorCondition <- function(x, ...) {
  x_bkp <- x
  x <- unclass(x)
  args <- list(x$message)
  args$class <- setdiff(class(x_bkp), c("error", "condition"))
  if (!length(args$class)) args$class <- NULL
  args$call <- x$call
  args <- c(args, x[-(1:2)])
  code <- .cstr_apply(args, "errorCondition", ...)
  repair_attributes_error(x_bkp, code, ...)
}

repair_attributes_error <- function(x, code, pipe = NULL, ...) {
  cl <- class(x)
  cl_is_idiomatic <- identical(tail(cl, 2), c("error", "condition"))
  .cstr_repair_attributes(
    x, code, pipe,
    idiomatic_class = if(cl_is_idiomatic) cl,
    ...
  )
}

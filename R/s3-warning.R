#' @export
#' @rdname other-opts
opts_warning <- function(constructor = c("warningCondition", "next"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("warning", constructor = constructor)
}

#' @export
.cstr_construct.warning <- function(x, opts = NULL, ...) {
  opts_local <- opts$warning %||% opts_warning()
  if (is_corrupted_warning(x) || opts_local[["constructor"]] == "next") return(NextMethod())
  constructor <- constructors$warning[[opts_local[["constructor"]]]]
  constructor(x, opts = opts, ...)
}

is_corrupted_warning <- function(x) {
  !(is.list(x) && identical(names(x)[1:2], c("message", "call")))
}

#' @export
constructors$warning$warningCondition <- function(x, ...) {
  x_bkp <- x
  x <- unclass(x)
  args <- list(x$message)
  args$class <- setdiff(class(x_bkp), c("warning", "condition"))
  if (!length(args$class)) args$class <- NULL
  args$call <- x$call
  args <- c(args, x[-(1:2)])
  code <- .cstr_apply(args, "warningCondition", ...)
  repair_attributes_warning(x_bkp, code, ...)
}

repair_attributes_warning <- function(x, code, pipe = NULL, ...) {
  cl <- class(x)
  cl_is_idiomatic <- identical(tail(cl, 2), c("warning", "condition"))
  .cstr_repair_attributes(
    x, code, pipe,
    idiomatic_class = if(cl_is_idiomatic) cl,
    ...
  )
}

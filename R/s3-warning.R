#' @export
#' @rdname other-opts
opts_warning <- function(constructor = c("warningCondition", "next"), ...) {
  .cstr_options("warning", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct warning
.cstr_construct.warning <- function(x, ...) {
  opts <- list(...)$opts$warning %||% opts_warning()
  if (is_corrupted_warning(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.warning", structure(NA, class = opts$constructor))
}

is_corrupted_warning <- function(x) {
  !(is.list(x) && identical(names(x)[1:2], c("message", "call")))
}

#' @export
#' @method .cstr_construct.warning warningCondition
.cstr_construct.warning.warningCondition <- function(x, ...) {
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

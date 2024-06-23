# This reproduces the aes() call, note that due to NSE we cannot guarantee that
# the variables will be found in caller env, and it would be costly and unsafe to
# eval the expressions or components

#' @export
#' @rdname other-opts
opts_uneval <- function(constructor = c("aes", "next", "list"), ...) {
  .cstr_options("uneval", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct uneval
.cstr_construct.uneval <- function(x, ...) {
  opts <- list(...)$opts$uneval %||% opts_uneval()
  if (is_corrupted_uneval(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.uneval", structure(NA, class = opts$constructor))
}

#' @export
#' @method .cstr_construct.uneval list
.cstr_construct.uneval.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
#' @method .cstr_construct.uneval aes
.cstr_construct.uneval.aes <- function(x, ...) {
  if (!length(x)) {
    return(
      repair_attributes_uneval(x, "ggplot2::aes()", ...)
    )
  }
  args <- lapply(x, function(x) rlang::expr_deparse(rlang::quo_squash(x)))
  nm1 <- names(args)[1]
  # omit `x` and `y` if provided in this order
  if (nm1 == "x") {
    names(args)[1] <- ""
    nm2 <- names(args)[2]
    if (!is.na(nm2) && nm2 == "y") {
      names(args)[2] <- ""
    }
  }
  code <- .cstr_apply(args, fun = "ggplot2::aes", recurse = FALSE, new_line = FALSE)
  repair_attributes_uneval(x, code, ...)
}

is_corrupted_uneval <- function(x) {
  # TODO
  FALSE
}

repair_attributes_uneval <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe,
    idiomatic_class = "uneval",
    ...
  )
}

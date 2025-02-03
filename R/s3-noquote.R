#' @export
#' @rdname other-opts
opts_noquote <- function(constructor = c("noquote", "next"), ...) {
  .cstr_options("noquote", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct noquote
.cstr_construct.noquote <- function(x, ...) {
  opts <- list(...)$opts$noquote %||% opts_noquote()
  if (is_corrupted_noquote(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.noquote", structure(NA, class = opts$constructor))
}

is_corrupted_noquote <- function(x) {
  # cannot really be corrupted because we just proviode a class, but makes
  # no sense to call `noquote()` if we have to fix the class next
  cl <- tail(class(x), 1)
  !(cl == "noquote" && (is.null(names(cl)) || names(cl) == "right"))
}

#' @export
#' @method .cstr_construct.noquote noquote
.cstr_construct.noquote.noquote <- function(x, ...) {
  right <- identical(tail(names(class(x)), 1), "right")
  x_bkp <- x
  class(x) <- setdiff(class(x), "noquote")
  args <- list(x)
  args$right <- if (right) TRUE
  code <- .cstr_apply(args, "noquote", ...)
  repair_attributes_noquote(x_bkp, code, ...)
}

repair_attributes_noquote <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe = pipe,
    idiomatic_class = class(x),
    ...
  )
}

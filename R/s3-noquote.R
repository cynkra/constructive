#' @export
#' @rdname other-opts
opts_noquote <- function(constructor = c("noquote", "next"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("noquote", constructor = constructor)
}

#' @export
.cstr_construct.noquote <- function(x, opts = NULL, ...) {
  opts_local <- opts$noquote %||% opts_noquote()
  if (is_corrupted_noquote(x) || opts_local[["constructor"]] == "next") return(NextMethod())
  constructor <- constructors$noquote[[opts_local[["constructor"]]]]
  constructor(x, opts = opts, ...)
}

is_corrupted_noquote <- function(x) {
  # cannot really be corrupted because we just proviode a class, but makes
  # no sense to call `noquote()` if we have to fix the class next
  cl <- tail(class(x), 1)
  !(cl == "noquote" && (is.null(names(cl)) || names(cl) == "right"))
}

#' @export
constructors$noquote$noquote <- function(x, ...) {
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
    x, code, pipe,
    idiomatic_class = class(x),
    ...
  )
}

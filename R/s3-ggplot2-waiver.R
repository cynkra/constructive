#' @export
#' @rdname other-opts
opts_waiver <- function(constructor = c("waiver", "next", "list"), ...) {
  .cstr_options("waiver", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct waiver
.cstr_construct.waiver <- function(x, ...) {
  opts <- list(...)$opts$waiver %||% opts_waiver()
  if (is_corrupted_waiver(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.waiver", structure(NA, class = opts$constructor))
}

is_corrupted_waiver <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.waiver list
.cstr_construct.waiver.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
#' @method .cstr_construct.waiver waiver
.cstr_construct.waiver.waiver <- function(x, ...) {
  code <- "ggplot2::waiver()"
  repair_attributes_waiver(x, code, ...)
}

repair_attributes_waiver <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = "waiver", ...)
}

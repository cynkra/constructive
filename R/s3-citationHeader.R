#' @export
#' @rdname other-opts
opts_citationHeader <- function(constructor = c("citHeader", "next"), ...) {
  .cstr_options("citationHeader", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct citationHeader
.cstr_construct.citationHeader <- function(x, ...) {
  opts <- list(...)$opts$citationHeader %||% opts_citationHeader()
  if (is_corrupted_citationHeader(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.citationHeader", structure(NA, class = opts$constructor))
}

is_corrupted_citationHeader <- function(x) {
  !is.character(x) || with_versions(R > "4.3.0")
}

#' @export
#' @method .cstr_construct.citationHeader citHeader
.cstr_construct.citationHeader.citHeader <- function(x, ...) {
  code <- .cstr_apply(list(unclass(x)), "citHeader", ...)
  repair_attributes_citationHeader(x, code, ...)
}

repair_attributes_citationHeader <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe = pipe,
    idiomatic_class = "citationHeader",
    ...
  )
}

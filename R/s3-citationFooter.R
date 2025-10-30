#' @export
#' @rdname other-opts
opts_citationFooter <- function(constructor = c("citFooter", "next"), ...) {
  .cstr_options("citationFooter", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct citationFooter
.cstr_construct.citationFooter <- function(x, ...) {
  opts <- list(...)$opts$citationFooter %||% opts_citationFooter()
  if (is_corrupted_citationFooter(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.citationFooter", structure(NA, class = opts$constructor))
}

is_corrupted_citationFooter <- function(x) {
  !is.character(x) || with_versions(R > "4.3.0")
}

#' @export
#' @method .cstr_construct.citationFooter citFooter
.cstr_construct.citationFooter.citFooter <- function(x, ...) {
  code <- .cstr_apply(list(unclass(x)), "citFooter", ...)
  repair_attributes_citationFooter(x, code, ...)
}

repair_attributes_citationFooter <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe = pipe,
    idiomatic_class = "citationFooter",
    ...
  )
}

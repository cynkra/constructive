#' @export
#' @rdname other-opts
opts_citationHeader <- function(constructor = c("citHeader", "next"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("citationHeader", constructor = constructor)
}

#' @export
.cstr_construct.citationHeader <- function(x, ...) {
  opts <- .cstr_fetch_opts("citationHeader", ...)
  if (is_corrupted_citationHeader(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$citationHeader[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_citationHeader <- function(x) {
  !is.character(x) || getRversion() > "4.3.0"
}

#' @export
constructors$citationHeader$citHeader <- function(x, ...) {
  code <- .cstr_apply(list(unclass(x)), "citHeader", ...)
  repair_attributes_citationHeader(x, code, ...)
}

repair_attributes_citationHeader <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe,
    idiomatic_class = "citationHeader",
    ...
  )
}

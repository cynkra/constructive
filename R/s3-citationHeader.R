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
.cstr_construct.citationHeader <- function(x, opts = NULL, ...) {
  opts_local <- opts$citationHeader %||% opts_citationHeader()
  if (is_corrupted_citationHeader(x) || opts_local[["constructor"]] == "next") return(NextMethod())
  constructor <- constructors$citationHeader[[opts_local[["constructor"]]]]
  constructor(x, opts = opts, ...)
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

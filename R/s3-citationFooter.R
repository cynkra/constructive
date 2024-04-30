#' @export
#' @rdname other-opts
opts_citationFooter <- function(constructor = c("citFooter", "next"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("citationFooter", constructor = constructor)
}

#' @export
.cstr_construct.citationFooter <- function(x, ...) {
  opts <- .cstr_fetch_opts("citationFooter", ...)
  if (is_corrupted_citationFooter(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$citationFooter[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_citationFooter <- function(x) {
  !is.character(x) || getRversion() > "4.3.0"
}

#' @export
constructors$citationFooter$citFooter <- function(x, ...) {
  code <- .cstr_apply(list(unclass(x)), "citFooter", ...)
  repair_attributes_citationFooter(x, code, ...)
}

repair_attributes_citationFooter <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe,
    idiomatic_class = "citationFooter",
    ...
  )
}

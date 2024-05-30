constructors$waiver <- new.env()

#' @export
#' @rdname other-opts
opts_waiver <- function(constructor = c("waiver", "next", "list"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("waiver", constructor = constructor)
}

#' @export
.cstr_construct.waiver <- function(x, opts = NULL, ...) {
  opts_local <- opts$waiver %||% opts_waiver()
  if (is_corrupted_waiver(x) || opts_local[["constructor"]] == "next") return(NextMethod())
  constructor <- constructors$waiver[[opts_local[["constructor"]]]]
  constructor(x, opts = opts, ...)
}

is_corrupted_waiver <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$waiver$list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
constructors$waiver$waiver <- function(x, ...) {
  code <- "ggplot2::waiver()"
  repair_attributes_waiver(x, code, ...)
}

repair_attributes_waiver <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = "waiver", ...)
}

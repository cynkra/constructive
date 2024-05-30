#' @export
#' @rdname other-opts
opts_difftime <- function(constructor = c("as.difftime", "next"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("difftime", constructor = constructor)
}

#' @export
.cstr_construct.difftime <- function(x, opts = NULL, ...) {
  opts_local <- opts$difftime %||% opts_difftime()
  if (is_corrupted_difftime(x) || opts_local[["constructor"]] == "next") return(NextMethod())
  constructor <- constructors$difftime[[opts_local[["constructor"]]]]
  constructor(x, opts = opts, ...)
}

is_corrupted_difftime <- function(x) {
  units <- attr(x, "units")
  !(
    mode(x) == "numeric" &&
      is.character(units) &&
      length(units) == 1 &&
      units %in% c("secs", "mins", "hours", "days", "weeks")
  )
}

#' @export
constructors$difftime$as.difftime <- function(x, ...) {
  x_bkp <- x
  attributes(x) <- attributes(x)["names"]
  args <- list(x, units = attr(x_bkp, "units"))
  code <- .cstr_apply(args, "as.difftime", ...)
  repair_attributes_difftime(x_bkp, code, ...)
}

repair_attributes_difftime <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe,
    idiomatic_class = "difftime",
    ignore = "units",
    ...
  )
}

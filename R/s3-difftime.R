#' @export
#' @rdname other-opts
opts_difftime <- function(constructor = c("as.difftime", "next"), ...) {
  .cstr_options("difftime", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct difftime
.cstr_construct.difftime <- function(x, ...) {
  opts <- list(...)$opts$difftime %||% opts_difftime()
  if (is_corrupted_difftime(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.difftime", structure(NA, class = opts$constructor))
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
#' @method .cstr_construct.difftime as.difftime
.cstr_construct.difftime.as.difftime <- function(x, ...) {
  x_bkp <- x
  attributes(x) <- attributes(x)["names"]
  args <- list(x, units = attr(x_bkp, "units"))
  code <- .cstr_apply(args, "as.difftime", ...)
  repair_attributes_difftime(x_bkp, code, ...)
}

repair_attributes_difftime <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe = pipe,
    idiomatic_class = "difftime",
    ignore = "units",
    ...
  )
}

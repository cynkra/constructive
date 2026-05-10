#' @export
#' @rdname other-opts
opts_NULL <- function(
    constructor = "NULL",
    ...) {
  .cstr_options(
    "NULL",
    constructor = constructor,
    ...
  )
}

#' @export
#' @method .cstr_construct NULL
.cstr_construct.NULL <- function(x, ...) {
  opts <- list(...)$opts$`NULL` %||% opts_NULL()
  if (is_corrupted_NULL(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.NULL", structure(NA, class = opts$constructor))
}

is_corrupted_NULL <- function(x) {
  typeof(x) != "NULL"
}

#' @export
#' @method .cstr_construct.NULL NULL
.cstr_construct.NULL.NULL <- function(x, ...) {
  "NULL"
}


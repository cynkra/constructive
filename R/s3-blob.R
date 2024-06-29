#' Constructive options for class 'blob'
#'
#' These options will be used on objects of class 'blob'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"blob"` (default): Use `blob::blob()` on a raw object.
#'   * `"new_blob"` (default): Use `blob::new_blob()` on a list of raw objects.
#' * `"as.blob"` : Use `blob::as_blob()` on a character vector
#'
#' Use `opts_raw()` and `opts_character()` to tweak the construction of raw or
#' character objects constructed as part of the blob construction.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_blob>
#' @export
opts_blob <- function(constructor = c("blob", "next"), ...) {
  .cstr_options("blob", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct blob
.cstr_construct.blob <- function(x, ...) {
  opts <- list(...)$opts$blob %||% opts_blob()
  if (is_corrupted_blob(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.blob", structure(NA, class = opts$constructor))
}

is_corrupted_blob <- function(x) {
  !is.list(x) || !all(vapply(x, is.raw, logical(1)))
}

#' @export
#' @method .cstr_construct.blob blob
.cstr_construct.blob.blob <- function(x, ...) {
  opts <- list(...)$opts$blob %||% opts_blob()
  code <- .cstr_apply(strip(x), fun = "blob::blob", ...)
  repair_attributes_blob(x, code, ...)
}

#' @export
#' @method .cstr_construct.blob new_blob
.cstr_construct.blob.new_blob <- function(x, ...) {
  opts <- list(...)$opts$blob %||% opts_blob()
  code <- .cstr_apply(list(strip(x)), fun = "blob::new_blob", ...)
  repair_attributes_blob(x, code, ...)
}

#' @export
#' @method .cstr_construct.blob as_blob
.cstr_construct.blob.as_blob <- function(x, ...) {
  any_zero_raw <- any(vapply(x, function(elt) raw(1) %in% elt, logical(1)))
  if (any_zero_raw) return(.cstr_construct.blob.blob(x, ...))
  opts <- list(...)$opts$blob %||% opts_blob()
  arg <- vapply(strip(x), rawToChar, character(1))
  code <- .cstr_apply(list(arg), fun = "blob::as_blob", ...)
  repair_attributes_blob(x, code, ...)
}

repair_attributes_blob <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("blob", "vctrs_list_of", "vctrs_vctr", "list"),
    ignore = if (identical(attr(x, "ptype"), raw(0))) "ptype"
  )
}

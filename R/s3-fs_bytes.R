#' Constructive options for class 'fs_bytes'
#'
#' These options will be used on objects of class 'fs_bytes'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as_fs_bytes"` (default): Build the object using `fs::as_fs_bytes()` on a
#'   numeric vector.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_fs_bytes>
#' @export
opts_fs_bytes <- function(constructor = c("as_fs_bytes", "next"), ...) {
  .cstr_options("fs_bytes", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct fs_bytes
.cstr_construct.fs_bytes <- function(x, ...) {
  opts <- list(...)$opts$fs_bytes %||% opts_fs_bytes()
  if (is_corrupted_fs_bytes(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.fs_bytes", structure(NA, class = opts$constructor))
}

is_corrupted_fs_bytes <- function(x) {
  !typeof(x) %in% c("double", "integer")
}

#' @export
#' @method .cstr_construct.fs_bytes as_fs_bytes
.cstr_construct.fs_bytes.as_fs_bytes <- function(x, ...) {
  code <- .cstr_apply(list(strip(x)), "fs::as_fs_bytes", ...)
  repair_attributes_fs_bytes(x, code, ...)
}

repair_attributes_fs_bytes <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("fs_bytes", "numeric")
  )
}

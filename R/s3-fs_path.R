#' Constructive options for class 'fs_path'
#'
#' These options will be used on objects of class 'fs_path'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"path"` (default): Build the object using `fs::path()` on a character
#'   vector.
#' * `"as_fs_path"` : Build the object using `fs::as_fs_path()` on a character
#'   vector.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' Both `fs::path()` and `fs::as_fs_path()` tidy their input, so paths that are
#' not tidy (e.g. `"a//b/"`) are always constructed with the `"next"` constructor.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_fs_path>
#' @export
opts_fs_path <- function(constructor = c("path", "as_fs_path", "next"), ...) {
  .cstr_options("fs_path", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct fs_path
.cstr_construct.fs_path <- function(x, ...) {
  opts <- list(...)$opts$fs_path %||% opts_fs_path()
  if (is_corrupted_fs_path(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.fs_path", structure(NA, class = opts$constructor))
}

is_corrupted_fs_path <- function(x) {
  if (!is.character(x)) return(TRUE)
  chr <- as.character(x)
  !identical(as.character(fs::path_tidy(chr)), chr)
}

#' @export
#' @method .cstr_construct.fs_path path
.cstr_construct.fs_path.path <- function(x, ...) {
  code <- .cstr_apply(list(as.character(x)), "fs::path", ...)
  repair_attributes_fs_path(x, code, ...)
}

#' @export
#' @method .cstr_construct.fs_path as_fs_path
.cstr_construct.fs_path.as_fs_path <- function(x, ...) {
  code <- .cstr_apply(list(as.character(x)), "fs::as_fs_path", ...)
  repair_attributes_fs_path(x, code, ...)
}

repair_attributes_fs_path <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("fs_path", "character"),
    repair_names = TRUE
  )
}

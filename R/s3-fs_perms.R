#' Constructive options for class 'fs_perms'
#'
#' These options will be used on objects of class 'fs_perms'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as_fs_perms"` (default): Build the object using `fs::as_fs_perms()` on
#'   octal strings such as `"644"`. Vectors of length other than 1 or 2 are
#'   first converted with `as.octmode()`, because `fs::as_fs_perms()` scrambles
#'   the order of longer character vectors.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' Objects containing `NA` or negative values are always constructed with the
#' `"next"` constructor.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_fs_perms>
#' @export
opts_fs_perms <- function(constructor = c("as_fs_perms", "next"), ...) {
  .cstr_options("fs_perms", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct fs_perms
.cstr_construct.fs_perms <- function(x, ...) {
  opts <- list(...)$opts$fs_perms %||% opts_fs_perms()
  if (is_corrupted_fs_perms(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.fs_perms", structure(NA, class = opts$constructor))
}

is_corrupted_fs_perms <- function(x) {
  !is.integer(x) || anyNA(x) || any(as.integer(x) < 0)
}

#' @export
#' @method .cstr_construct.fs_perms as_fs_perms
.cstr_construct.fs_perms.as_fs_perms <- function(x, ...) {
  octal <- sprintf("%o", as.integer(x))
  if (length(octal) %in% 1:2) {
    code <- .cstr_apply(list(octal), "fs::as_fs_perms", ...)
  } else {
    # fs::as_fs_perms() scrambles character vectors of length > 2
    code <- .cstr_apply(list(octal), "as.octmode", ...)
    code <- .cstr_apply(list(code), "fs::as_fs_perms", ..., recurse = FALSE)
  }
  repair_attributes_fs_perms(x, code, ...)
}

repair_attributes_fs_perms <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("fs_perms", "integer"),
    repair_names = TRUE
  )
}

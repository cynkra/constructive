#' Constructive options for class 'integer64'
#'
#' These options will be used on objects of class 'integer64'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as.integer64"` (default): Build the object using `as.integer64()` on a
#'   character vector.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"double"` : We define as an atomic vector and repair attributes.
#'
#' We don't recommend the "next" and "double" constructors for this class as
#' they give incorrect results on negative or `NA` "integer64" objects
#' due to some quirks in the implementation of the 'bit64' package.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_integer64>
#' @export
opts_integer64 <- function(constructor = c("as.integer64", "next", "double"), ...) {
  .cstr_options("integer64", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct integer64
.cstr_construct.integer64 <- function(x, ...) {
  opts <- list(...)$opts$integer64 %||% opts_integer64()
  if (is_corrupted_integer64(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.integer64", structure(NA, class = opts$constructor))
}

is_corrupted_integer64 <- function(x) {
  typeof(x) != "double"
}

#' @export
#' @method .cstr_construct.integer64 double
.cstr_construct.integer64.double <- function(x, ...) {
  .cstr_construct.double(x, ...)
}

#' @export
#' @method .cstr_construct.integer64 as.integer64
.cstr_construct.integer64.as.integer64 <- function(x, ...) {
  chr <- trimws(format(x))
  # bit64 >= 4.8 warns when coercing the string "NA", but not `NA`.
  # integer64 NAs are encoded as the double -0, equal to 0 but with 1 / x = -Inf.
  # `==` and `/` are our shims, they work on the unclassed doubles so we don't
  # depend on bit64's methods. Values whose doubles are NaN give NA, dropped by which().
  chr[which(x == 0 & 1 / x == -Inf)] <- NA
  code <- .cstr_apply(list(chr), "bit64::as.integer64", ...)
  repair_attributes_integer64(x, code, ...)
}

repair_attributes_integer64 <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "integer64"
  )
}

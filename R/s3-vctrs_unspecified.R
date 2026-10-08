#' Constructive options for class 'vctrs_unspecified'
#'
#' These options will be used on objects of class 'vctrs_unspecified'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"unspecified"` (default): Use `vctrs::unspecified()` on the length of the
#'   object.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_vctrs_unspecified>
#' @export
opts_vctrs_unspecified <- function(constructor = c("unspecified", "next"), ...) {
  .cstr_options("vctrs_unspecified", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct vctrs_unspecified
.cstr_construct.vctrs_unspecified <- function(x, ...) {
  opts <- list(...)$opts$vctrs_unspecified %||% opts_vctrs_unspecified()
  if (is_corrupted_vctrs_unspecified(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.vctrs_unspecified", structure(NA, class = opts$constructor))
}

is_corrupted_vctrs_unspecified <- function(x) {
  typeof(x) != "logical" || !all(is.na(x))
}

#' @export
#' @method .cstr_construct.vctrs_unspecified unspecified
.cstr_construct.vctrs_unspecified.unspecified <- function(x, ...) {
  code <- .cstr_apply(list(as.double(length(x))), "vctrs::unspecified", ..., new_line = FALSE)
  repair_attributes_vctrs_unspecified(x, code, ...)
}

repair_attributes_vctrs_unspecified <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "vctrs_unspecified",
    repair_names = TRUE
  )
}

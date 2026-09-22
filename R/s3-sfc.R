#' Constructive options for class 'sfc'
#'
#' These options will be used on objects of class 'sfc', simple feature
#' geometry list-columns from the 'sf' package.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"st_sfc"` (default): We build the object using `sf::st_sfc()` on the
#'   geometries, with `crs` and `precision` arguments if relevant. The other
#'   attributes ("bbox", "n_empty", "classes"...) are computed by `sf::st_sfc()`
#'   and are repaired only if they don't match.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_sfc>
#' @export
opts_sfc <- function(constructor = c("st_sfc", "next"), ...) {
  .cstr_options("sfc", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct sfc
.cstr_construct.sfc <- function(x, ...) {
  opts <- list(...)$opts$sfc %||% opts_sfc()
  if (is_corrupted_sfc(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.sfc", structure(NA, class = opts$constructor))
}

is_corrupted_sfc <- function(x) {
  if (!is.list(x) || !all(vapply(x, inherits, logical(1), "sfg"))) return(TRUE)
  precision <- attr(x, "precision")
  if (!is.null(precision) && !(is.numeric(precision) && length(precision) == 1)) return(TRUE)
  crs <- attr(x, "crs")
  !is.null(crs) && !inherits(crs, "crs")
}

#' @export
#' @method .cstr_construct.sfc st_sfc
.cstr_construct.sfc.st_sfc <- function(x, ...) {
  args <- strip(x)
  names(args) <- NULL
  args$crs <- crs_arg_sf(attr(x, "crs"))
  precision <- attr(x, "precision")
  if (!identical(precision, 0)) args$precision <- precision
  code <- .cstr_apply(args, "sf::st_sfc", ...)
  repair_attributes_sfc(x, code, ..., reference = do.call(sf::st_sfc, args))
}

repair_attributes_sfc <- function(x, code, ..., reference) {
  repair_attributes_from_reference(x, code, ..., reference = reference)
}

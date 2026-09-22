#' Constructive options for class 'bbox'
#'
#' These options will be used on objects of class 'bbox'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"st_bbox"` (default): We build the object using `sf::st_bbox()` on a
#'   named double vector, with a `crs` argument if relevant.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_bbox>
#' @export
opts_bbox <- function(constructor = c("st_bbox", "next"), ...) {
  .cstr_options("bbox", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct bbox
.cstr_construct.bbox <- function(x, ...) {
  opts <- list(...)$opts$bbox %||% opts_bbox()
  if (is_corrupted_bbox(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.bbox", structure(NA, class = opts$constructor))
}

is_corrupted_bbox <- function(x) {
  !is.double(x) || !identical(names(x), c("xmin", "ymin", "xmax", "ymax"))
}

#' @export
#' @method .cstr_construct.bbox st_bbox
.cstr_construct.bbox.st_bbox <- function(x, ...) {
  args <- list(strip(x))
  args$crs <- crs_arg_sf(attr(x, "crs"))
  code <- .cstr_apply(args, "sf::st_bbox", ..., new_line = FALSE)
  repair_attributes_bbox(x, code, ..., reference = do.call(sf::st_bbox, args))
}

repair_attributes_bbox <- function(x, code, ..., reference) {
  repair_attributes_from_reference(x, code, ..., reference = reference)
}

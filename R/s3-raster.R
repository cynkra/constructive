#' Constructive options for class 'raster'
#'
#' These options will be used on objects of class 'raster'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as.raster"` (default): We build the object using `as.raster()` on a
#'   character matrix of colors.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `opts_matrix("matrix")`, note that the colors of a raster
#'   are stored row by row.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_raster>
#' @export
opts_raster <- function(constructor = c("as.raster", "next"), ...) {
  .cstr_options("raster", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct raster
.cstr_construct.raster <- function(x, ...) {
  opts <- list(...)$opts$raster %||% opts_raster()
  if (is_corrupted_raster(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.raster", structure(NA, class = opts$constructor))
}

is_corrupted_raster <- function(x) {
  dim <- attr(x, "dim")
  !is.character(x) || !is.integer(dim) || length(dim) != 2
}

#' @export
#' @method .cstr_construct.raster as.raster
.cstr_construct.raster.as.raster <- function(x, ...) {
  dim <- attr(x, "dim")
  x_stripped <- x
  attributes(x_stripped) <- NULL
  # as.raster() transposes the matrix, colors are stored row by row
  m <- matrix(x_stripped, dim[[1]], dim[[2]], byrow = TRUE)
  code <- .cstr_apply(list(m), "as.raster", ...)
  repair_attributes_raster(x, code, ...)
}

repair_attributes_raster <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "raster",
    ignore = "dim"
  )
}

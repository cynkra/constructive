#' Constructive options for class 'sfg'
#'
#' These options will be used on objects of class 'sfg', simple feature
#' geometries from the 'sf' package.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"st_sfg"` (default): We build the object using the sf constructor
#'   matching the geometry type, i.e. `sf::st_point()`, `sf::st_multipoint()`,
#'   `sf::st_linestring()`, `sf::st_multilinestring()`, `sf::st_polygon()`,
#'   `sf::st_multipolygon()` or `sf::st_geometrycollection()`, with a `dim`
#'   argument when the dimension can't be inferred from the coordinates.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' Other geometry types, such as "CIRCULARSTRING", don't have a dedicated
#' constructor and are built with the next constructor.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_sfg>
#' @export
opts_sfg <- function(constructor = c("st_sfg", "next"), ...) {
  .cstr_options("sfg", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct sfg
.cstr_construct.sfg <- function(x, ...) {
  opts <- list(...)$opts$sfg %||% opts_sfg()
  if (is_corrupted_sfg(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.sfg", structure(NA, class = opts$constructor))
}

is_corrupted_sfg <- function(x) {
  cl <- oldClass(x)
  if (length(cl) != 3 || !cl[[2]] %in% names(sfg_constructors)) return(TRUE)
  # the constructors validate the coordinates and set the class
  reference <- try(do.call(sfg_constructor(x), sfg_args(x)), silent = TRUE)
  !identical(oldClass(reference), cl)
}

#' @export
#' @method .cstr_construct.sfg st_sfg
.cstr_construct.sfg.st_sfg <- function(x, ...) {
  fun <- sfg_constructor(x)
  args <- sfg_args(x)
  reference <- do.call(fun, args)
  args <- keep_only_non_defaults(args, fun)
  names(args)[names(args) == "x"] <- ""
  code <- .cstr_apply(args, paste0("sf::", sfg_constructors[[oldClass(x)[[2]]]]), ...)
  repair_attributes_sfg(x, code, ..., reference = reference)
}

repair_attributes_sfg <- function(x, code, ..., reference) {
  repair_attributes_from_reference(x, code, ..., reference = reference)
}

sfg_constructors <- c(
  POINT = "st_point",
  MULTIPOINT = "st_multipoint",
  LINESTRING = "st_linestring",
  MULTILINESTRING = "st_multilinestring",
  POLYGON = "st_polygon",
  MULTIPOLYGON = "st_multipolygon",
  GEOMETRYCOLLECTION = "st_geometrycollection"
)

sfg_constructor <- function(x) {
  getExportedValue("sf", sfg_constructors[[oldClass(x)[[2]]]])
}

# The constructors only add a class to the coordinates, other attributes are
# repaired afterwards. The dimension is inferred from the number of coordinates,
# or from the elements for geometry collections, we provide it explicitly only
# if necessary
sfg_args <- function(x) {
  fun <- sfg_constructor(x)
  coords <- strip(x)
  attr(coords, "dim") <- attr(x, "dim")
  args <- list(x = coords)
  if (!identical(oldClass(do.call(fun, args)), oldClass(x))) {
    dim_arg <- if (oldClass(x)[[2]] == "GEOMETRYCOLLECTION") "dims" else "dim"
    args[[dim_arg]] <- oldClass(x)[[1]]
  }
  args
}

#' Constructive options for class 'crs'
#'
#' These options will be used on objects of class 'crs'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"st_crs"` (default): We build the object using `sf::st_crs()` on an EPSG
#'   code if possible (e.g. `sf::st_crs(4326)`), or else on the user input
#'   (e.g. `sf::st_crs("OGC:CRS84")`) or on the wkt definition, the first that
#'   reproduces the object exactly.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' The wkt definition of a crs depends on the installed PROJ version, so a crs
#' object created on a different setup might not be reproducible with
#' `sf::st_crs()`, in this case we fall back to the next constructor.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_crs>
#' @export
opts_crs <- function(constructor = c("st_crs", "next"), ...) {
  .cstr_options("crs", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct crs
.cstr_construct.crs <- function(x, ...) {
  opts <- list(...)$opts$crs %||% opts_crs()
  if (is_corrupted_crs(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.crs", structure(NA, class = opts$constructor))
}

is_corrupted_crs <- function(x) {
  is.null(crs_arg(x))
}

#' @export
#' @method .cstr_construct.crs st_crs
.cstr_construct.crs.st_crs <- function(x, ...) {
  code <- .cstr_apply(list(crs_arg(x)), "sf::st_crs", ..., new_line = FALSE)
  repair_attributes_crs(x, code, ...)
}

repair_attributes_crs <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "crs"
  )
}

# The simplest input that `sf::st_crs()` takes to reproduce `x` exactly, or
# `NULL` if there is none. Also used for the `crs` argument of other sf constructors.
crs_arg <- function(x) {
  if (!is.list(x) || !identical(names(x), c("input", "wkt"))) return(NULL)
  input <- x$input
  candidates <- list(input, x$wkt)
  if (rlang::is_string(input) && grepl("^EPSG:[0-9]+$", input)) {
    candidates <- c(list(as.numeric(sub("^EPSG:", "", input))), candidates)
  }
  if (identical(input, NA_character_)) candidates <- c(list(NA), candidates)
  for (candidate in candidates) {
    crs <- try(suppressWarnings(sf::st_crs(candidate)), silent = TRUE)
    if (identical(crs, x)) return(candidate)
  }
  NULL
}

# The `crs` argument of sf constructors, `NULL` if the default `NA` crs is used
crs_arg_sf <- function(crs) {
  if (is.null(crs) || identical(crs, sf::NA_crs_)) return(NULL)
  crs_arg(crs) %||% crs
}

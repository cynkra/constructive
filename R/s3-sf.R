#' Constructive options for class 'sf'
#'
#' These options will be used on objects of class 'sf', simple feature data
#' frames from the 'sf' package.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"st_sf"` (default): We build the object using `sf::st_sf()` on the columns,
#'   or on a tibble if the object is also a tibble, with `sf_column_name` and
#'   `agr` arguments if relevant.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' `sf::st_sf()` places the geometry columns last and doesn't support list
#' columns or names that clash with its arguments unless the input is a tibble,
#' in these cases we use the next constructor.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_sf>
#' @export
opts_sf <- function(constructor = c("st_sf", "next"), ...) {
  .cstr_options("sf", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct sf
.cstr_construct.sf <- function(x, ...) {
  opts <- list(...)$opts$sf %||% opts_sf()
  if (is_corrupted_sf(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.sf", structure(NA, class = opts$constructor))
}

is_corrupted_sf <- function(x) {
  if (is_corrupted_data.frame(x)) return(TRUE)
  sfc_lgl <- vapply(x, inherits, logical(1), "sfc")
  sf_column <- attr(x, "sf_column")
  if (!rlang::is_string(sf_column) || !isTRUE(sfc_lgl[sf_column])) return(TRUE)
  if (inherits(x, "tbl_df")) return(FALSE)
  # the columns are provided directly to `sf::st_sf()`
  if (is.unsorted(sfc_lgl)) return(TRUE)
  if (!all(vapply(x[!sfc_lgl], is.atomic, logical(1)))) return(TRUE)
  forbidden_names <- c(names(formals(sf::st_sf)), "", NA)
  any(names(x) %in% forbidden_names) || anyDuplicated(names(x))
}

#' @export
#' @method .cstr_construct.sf st_sf
.cstr_construct.sf.st_sf <- function(x, ...) {
  if (inherits(x, "tbl_df")) {
    args <- list(structure(x, class = setdiff(oldClass(x), "sf"), sf_column = NULL, agr = NULL))
  } else {
    args <- strip(x)
    row_names <- attr(x, "row.names")
    if (!identical(row_names, seq_along(row_names))) args$row.names <- row_names
  }
  sf_column <- attr(x, "sf_column")
  sfc_nms <- names(x)[vapply(x, inherits, logical(1), "sfc")]
  if (sf_column != sfc_nms[[1]]) args$sf_column_name <- sf_column
  agr <- attr(x, "agr")
  if (is.factor(agr) && !all(is.na(agr))) {
    args$agr <- setNames(as.character.factor(agr), names(agr))[!is.na(agr)]
  }
  code <- .cstr_apply(args, "sf::st_sf", ...)
  repair_attributes_sf(x, code, ..., reference = do.call(sf::st_sf, args))
}

repair_attributes_sf <- function(x, code, ..., reference) {
  repair_attributes_from_reference(x, code, ..., reference = reference)
}

#' Constructive options for class 'IDate'
#'
#' These options will be used on objects of class 'IDate'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as.IDate"` (default): We build the object using `data.table::as.IDate()`
#'   on a character vector, or on an integer vector if some dates cannot be
#'   formatted as `"YYYY-MM-DD"` strings (years outside of 1 to 9999).
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"integer"` : We define as an integer vector and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_IDate>
#' @export
opts_IDate <- function(constructor = c("as.IDate", "next", "integer"), ...) {
  .cstr_options("IDate", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct IDate
.cstr_construct.IDate <- function(x, ...) {
  opts <- list(...)$opts$IDate %||% opts_IDate()
  if (is_corrupted_IDate(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.IDate", structure(NA, class = opts$constructor))
}

is_corrupted_IDate <- function(x) {
  typeof(x) != "integer"
}

#' @export
#' @method .cstr_construct.IDate as.IDate
.cstr_construct.IDate.as.IDate <- function(x, ...) {
  # -719162 is "0001-01-01" and 2932896 is "9999-12-31"
  compatible_with_char <- all(is.na(x) | (x >= -719162L & x <= 2932896L))
  if (compatible_with_char) {
    arg <- unname(format.Date(x))
  } else {
    arg <- as.integer(x)
  }
  code <- .cstr_apply(list(arg), "data.table::as.IDate", ..., new_line = FALSE)
  repair_attributes_IDate(x, code, ...)
}

#' @export
#' @method .cstr_construct.IDate integer
.cstr_construct.IDate.integer <- function(x, ...) {
  .cstr_construct.integer(x, ...)
}

repair_attributes_IDate <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("IDate", "Date"),
    repair_names = TRUE
  )
}

#' Constructive options for class 'roman'
#'
#' These options will be used on objects of class 'roman'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as.roman"` (default): We build the object using `as.roman()` on a
#'   character vector of roman numerals.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `opts_integer()`.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_roman>
#' @export
opts_roman <- function(constructor = c("as.roman", "next"), ...) {
  .cstr_options("roman", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct roman
.cstr_construct.roman <- function(x, ...) {
  opts <- list(...)$opts$roman %||% opts_roman()
  if (is_corrupted_roman(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.roman", structure(NA, class = opts$constructor))
}

is_corrupted_roman <- function(x) {
  if (!is.integer(x)) return(TRUE)
  attributes(x) <- NULL
  # as.roman() sets values out of this range to NA
  any(x < 1L | x > 3999L, na.rm = TRUE)
}

#' @export
#' @method .cstr_construct.roman as.roman
.cstr_construct.roman.as.roman <- function(x, ...) {
  x_stripped <- x
  attributes(x_stripped) <- NULL
  # the as.character() method is not exported, we dispatch on a clean object
  x_chr <- base::as.character(structure(x_stripped, class = "roman"))
  code <- .cstr_apply(list(x_chr), "as.roman", ...)
  repair_attributes_roman(x, code, ...)
}

repair_attributes_roman <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "roman",
    repair_names = TRUE
  )
}

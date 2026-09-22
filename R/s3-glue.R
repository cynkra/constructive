#' Constructive options for class 'glue'
#'
#' These options will be used on objects of class 'glue'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as_glue"` (default): Use `glue::as_glue()` on a character vector.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' Use `opts_character()` to tweak the construction of the character vector
#' constructed as part of the glue construction.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_glue>
#' @export
opts_glue <- function(constructor = c("as_glue", "next"), ...) {
  .cstr_options("glue", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct glue
.cstr_construct.glue <- function(x, ...) {
  opts <- list(...)$opts$glue %||% opts_glue()
  if (is_corrupted_glue(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.glue", structure(NA, class = opts$constructor))
}

is_corrupted_glue <- function(x) {
  # `glue::as_glue()` converts strings to UTF-8
  typeof(x) != "character" || any(Encoding(x) %in% c("latin1", "bytes"))
}

#' @export
#' @method .cstr_construct.glue as_glue
.cstr_construct.glue.as_glue <- function(x, ...) {
  code <- .cstr_apply(list(strip(x)), "glue::as_glue", ...)
  repair_attributes_glue(x, code, ...)
}

repair_attributes_glue <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("glue", "character")
  )
}

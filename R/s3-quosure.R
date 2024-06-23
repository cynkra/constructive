#' Constructive options for class 'quosure'
#'
#' These options will be used on objects of class 'quosure'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"new_quosure"` (default): Build the object using a `new_quosure()` call on a
#' character vector.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"language"` : We define as an language object and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_quosure>
#' @export
opts_quosure <- function(constructor = c("new_quosure", "next", "language"), ...) {
  .cstr_options("quosure", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct quosure
.cstr_construct.quosure <- function(x, ...) {
  opts <- list(...)$opts[["quosure"]] %||% opts_quosure()
  if (is_corrupted_quosure(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.quosure", structure(NA, class = opts$constructor))
}

is_corrupted_quosure <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.quosure new_quosure
.cstr_construct.quosure.new_quosure <- function(x, env = NULL, ...) {
  if (identical(env, attr(x, ".Environment"))) {
    code <- .cstr_apply(list(rlang::quo_squash(x)), "rlang::new_quosure", ...)
  } else {
    code <- .cstr_apply(list(rlang::quo_squash(x), attr(x, ".Environment")), "rlang::new_quosure", ...)
  }
  repair_attributes_quosure(x, code, env = env, ...)
}

#' @export
#' @method .cstr_construct.quosure language
.cstr_construct.quosure.language <- function(x, ...) {
  .cstr_construct.language(x, ...)
}

repair_attributes_quosure <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = ".Environment",
    idiomatic_class = c("quosure", "formula")
  )
}

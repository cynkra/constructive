constructors$quosure <- new.env()

#' Constructive options for class 'quosure'
#'
#' These options will be used on objects of class 'quosure'.
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"new_quosure"` (default): Build the object using a `new_quosure()` call on a
#' character vector.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"language"` : We define as an language object and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @param origin Origin to be used, ignored when irrelevant.
#'
#' @return An object of class <constructive_options/constructive_options_factor>
#' @export
opts_quosure <- function(constructor = c("new_quosure", "next", "language"), ..., origin = "1970-01-01") {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  .cstr_options("quosure", constructor = constructor, origin = origin)
}

#' @export
.cstr_construct.quosure <- function(x, ...) {
  opts <- .cstr_fetch_opts("quosure", ...)
  if (is_corrupted_quosure(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$quosure[[opts$constructor]]
  constructor(x, ..., origin = opts$origin)
}

#' @export
is_corrupted_quosure <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$quosure$new_quosure <- function(x, env, ...) {
  if (identical(env, attr(x, ".Environment"))) {
    code <- .cstr_apply(list(rlang::quo_squash(x)), "rlang::new_quosure", ...)
  } else {
    code <- .cstr_apply(list(rlang::quo_squash(x), attr(x, ".Environment")), "rlang::new_quosure", ...)
  }
  repair_attributes.quosure(x, code, env = env, ...)
}

#' @export
constructors$quosure$language <- function(x, ...) {
  .cstr_construct.language(x, ...)
}

#' @export
repair_attributes.quosure <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = ".Environment",
    idiomatic_class = c("quosure", "formula")
  )
}

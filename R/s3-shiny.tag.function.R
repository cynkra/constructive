#' Constructive options for class 'shiny.tag.function'
#'
#' These options will be used on objects of class 'shiny.tag.function'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"tagFunction"` (default): We build the object using `htmltools::tagFunction()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' Use `opts_function()` to tweak the construction of the function.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_shiny.tag.function>
#' @export
opts_shiny.tag.function <- function(constructor = c("tagFunction", "next"), ...) {
  .cstr_options("shiny.tag.function", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct shiny.tag.function
.cstr_construct.shiny.tag.function <- function(x, ...) {
  opts <- list(...)$opts$shiny.tag.function %||% opts_shiny.tag.function()
  if (is_corrupted_shiny.tag.function(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.shiny.tag.function", structure(NA, class = opts$constructor))
}

is_corrupted_shiny.tag.function <- function(x) {
  !is.function(x) || length(formals(x)) != 0
}

#' @export
#' @method .cstr_construct.shiny.tag.function tagFunction
.cstr_construct.shiny.tag.function.tagFunction <- function(x, ...) {
  # other attributes are repaired on the output of `htmltools::tagFunction()`
  fun <- x
  attributes(fun) <- NULL
  attr(fun, "srcref") <- attr(x, "srcref")
  code <- .cstr_apply(list(fun), fun = "htmltools::tagFunction", ...)
  repair_attributes_shiny.tag.function(x, code, ...)
}

repair_attributes_shiny.tag.function <- function(x, code, ...) {
  # the srcref is handled by the function's constructor
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "shiny.tag.function",
    ignore = "srcref"
  )
}

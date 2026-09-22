#' Constructive options for class 'shiny.tag.list'
#'
#' These options will be used on objects of class 'shiny.tag.list'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"tagList"` (default): We build the object using `htmltools::tagList()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_shiny.tag.list>
#' @export
opts_shiny.tag.list <- function(constructor = c("tagList", "next"), ...) {
  .cstr_options("shiny.tag.list", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct shiny.tag.list
.cstr_construct.shiny.tag.list <- function(x, ...) {
  opts <- list(...)$opts$shiny.tag.list %||% opts_shiny.tag.list()
  if (is_corrupted_shiny.tag.list(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.shiny.tag.list", structure(NA, class = opts$constructor))
}

is_corrupted_shiny.tag.list <- function(x) {
  # `htmltools::tagList()` always returns a named list, and forwards its dots to
  # `rlang::dots_list()` so names can't clash with the latter's formals
  nms <- names(x)
  !is.list(x) ||
    is.null(nms) ||
    anyNA(nms) ||
    any(nms %in% names(formals(rlang::dots_list)))
}

#' @export
#' @method .cstr_construct.shiny.tag.list tagList
.cstr_construct.shiny.tag.list.tagList <- function(x, ...) {
  code <- .cstr_apply(x, fun = "htmltools::tagList", ...)
  repair_attributes_shiny.tag.list(x, code, ...)
}

repair_attributes_shiny.tag.list <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("shiny.tag.list", "list")
  )
}

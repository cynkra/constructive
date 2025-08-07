#' Constructive options for class 'Guides'
#'
#' These options will be used on objects of class 'Guides'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"guides"` (default): We build the object using `ggplot2::guides()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_Guides>
#' @export
opts_Guides <- function(constructor = c("guides", "next"), ...) {
  constructive::.cstr_options("Guides", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.Guides <- function(x, ...) {
  opts <- list(...)$opts$Guides %||% opts_Guides()
  if (is_corrupted_Guides(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.Guides", structure(NA, class = opts$constructor))
}

is_corrupted_Guides <- function(x) {
  FALSE
}

#' @export
.cstr_construct.Guides.guides <- function(x, ...) {
  args <- x$guides
  code <- constructive::.cstr_apply(args, fun = "ggplot2::guides", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("Guides", "ggproto", "gg")
  )
}

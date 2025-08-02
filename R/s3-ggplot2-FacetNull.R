#' Constructive options for class 'FacetNull'
#'
#' These options will be used on objects of class 'FacetNull'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"facet_null"` (default): We build the object using `ggplot2::facet_null()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_FacetNull>
#' @export
opts_FacetNull <- function(constructor = c("facet_null", "next"), ...) {
  constructive::.cstr_options("FacetNull", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.FacetNull <- function(x, ...) {
  opts <- list(...)$opts$FacetNull %||% opts_FacetNull()
  if (is_corrupted_FacetNull(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.FacetNull", structure(NA, class = opts$constructor))
}

is_corrupted_FacetNull <- function(x) {
  FALSE
}

#' @export
.cstr_construct.FacetNull.facet_null <- function(x, ...) {
  args <- list()
  args$shrink = x$shrink
  args <- keep_only_non_defaults(args, ggplot2::facet_null)
  code <- constructive::.cstr_apply(args, fun = "ggplot2::facet_null", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("FacetNull", "Facet", "ggproto", "gg")
  )
}

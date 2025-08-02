#' Constructive options for class 'GuideBins'
#'
#' These options will be used on objects of class 'GuideBins'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"guide_bins"` (default): We build the object using `ggplot2::guide_bins()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_GuideBins>
#' @export
opts_GuideBins <- function(constructor = c("guide_bins", "next"), ...) {
  constructive::.cstr_options("GuideBins", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.GuideBins <- function(x, ...) {
  opts <- list(...)$opts$GuideBins %||% opts_GuideBins()
  if (is_corrupted_GuideBins(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.GuideBins", structure(NA, class = opts$constructor))
}

is_corrupted_GuideBins <- function(x) {
  !is.environment(x) ||
    !is.function(x$super) ||
    !identical(environment(x$super)$env$args$name, "bins")
}

#' @export
.cstr_construct.GuideBins.guide_bins <- function(x, ...) {
  if (identical(x, ggplot2::GuideBins)) return("ggplot2::GuideBins")
  args <- environment(x$super)$env$args # x$params
  args$name <- NULL
  if (!length(args$override.aes)) args$override.aes <- NULL
  args <- keep_only_non_defaults(args, ggplot2::guide_bins)
  code <- constructive::.cstr_apply(args, fun = "ggplot2::guide_bins", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("GuideBins", "GuideLegend", "Guide", "ggproto", "gg")
  )
}

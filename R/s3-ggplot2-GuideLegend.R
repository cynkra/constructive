#' Constructive options for class 'GuideLegend'
#'
#' These options will be used on objects of class 'GuideLegend'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"guide_legend"` (default): We build the object using `ggplot2::guide_legend()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_GuideLegend>
#' @export
opts_GuideLegend <- function(constructor = c("guide_legend", "next"), ...) {
  constructive::.cstr_options("GuideLegend", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.GuideLegend <- function(x, ...) {
  opts <- list(...)$opts$GuideLegend %||% opts_GuideLegend()
  if (is_corrupted_GuideLegend(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.GuideLegend", structure(NA, class = opts$constructor))
}

is_corrupted_GuideLegend <- function(x) {
  !is.environment(x) || !identical(x$params$name, "legend")
}

#' @export
.cstr_construct.GuideLegend.guide_legend <- function(x, ...) {
  if (identical(x, ggplot2::GuideLegend)) return("ggplot2::GuideLegend")
  args <- environment(x$super)$env$args # x$params
  args$name <- NULL
  if (!length(args$override.aes)) args$override.aes <- NULL
  args <- keep_only_non_defaults(args, ggplot2::guide_legend)
  code <- constructive::.cstr_apply(args, fun = "ggplot2::guide_legend", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("GuideLegend", "Guide", "ggproto", "gg")
  )
}

#' Constructive options for class 'GuideColourbar'
#'
#' These options will be used on objects of class 'GuideColourbar'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"guide_colorbar"` (default): We build the object using `ggplot2::guide_colorbar()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_GuideColourbar>
#' @export
opts_GuideColourbar <- function(constructor = c("guide_colorbar", "next"), ...) {
  constructive::.cstr_options("GuideColourbar", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.GuideColourbar <- function(x, ...) {
  opts <- list(...)$opts$GuideColourbar %||% opts_GuideColourbar()
  if (is_corrupted_GuideColourbar(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.GuideColourbar", structure(NA, class = opts$constructor))
}

is_corrupted_GuideColourbar <- function(x) {
  !is.environment(x) ||
    !is.function(x$super) ||
    !identical(environment(x$super)$env$args$name, "colourbar")
}

#' @export
.cstr_construct.GuideColourbar.guide_colorbar <- function(x, ...) {
  if (identical(x, ggplot2::GuideColourbar)) return("ggplot2::GuideColourbar")
  args <- environment(x$super)$env$args # x$params
  args$name <- NULL
  args$draw.llim <- args$draw_lim[[1]]
  args$draw.ulim <- args$draw_lim[[2]]
  args$draw_lim <- NULL

  if (args$display == "gradient" && args$nbin == 15) {
    args$nbin <- NULL
  } else   if (args$display != "gradient" && args$nbin == 300) {
    args$nbin <- NULL
  }

  if (!length(args$override.aes)) args$override.aes <- NULL
  args <- keep_only_non_defaults(args, ggplot2::guide_colorbar)
  code <- constructive::.cstr_apply(args, fun = "ggplot2::guide_colorbar", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("GuideColourbar", "GuideLegend", "Guide", "ggproto", "gg")
  )
}

#' Constructive options for class 'GuideCustom'
#'
#' These options will be used on objects of class 'GuideCustom'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"guide_custom"` (default): We build the object using `ggplot2::guide_custom()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_GuideCustom>
#' @export
opts_GuideCustom <- function(constructor = c("guide_custom", "next"), ...) {
  constructive::.cstr_options("GuideCustom", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.GuideCustom <- function(x, ...) {
  opts <- list(...)$opts$GuideCustom %||% opts_GuideCustom()
  if (is_corrupted_GuideCustom(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.GuideCustom", structure(NA, class = opts$constructor))
}

is_corrupted_GuideCustom <- function(x) {
  !is.environment(x) ||
    !is.function(x$super)
}

#' @export
.cstr_construct.GuideCustom.guide_custom <- function(x, ...) {
  if (identical(x, ggplot2::GuideCustom)) return("ggplot2::GuideCustom")
  args <- environment(x$super)$env$args # x$params
  args$hash <- NULL
  if (identical(args$height, grid::grobHeight(args$grob))) {
    args$height <- NULL
  }
  if (identical(args$width, grid::grobWidth(args$grob))) {
    args$width <- NULL
  }
  # don't name main arg
  names(args)[names(args) == "grob"] <- ""
  args <- keep_only_non_defaults(args, ggplot2::guide_colorsteps)
  code <- constructive::.cstr_apply(args, fun = "ggplot2::guide_custom", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("GuideCustom", "Guide", "ggproto", "gg")
  )
}

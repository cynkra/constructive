#' Constructive options for class 'GuideAxisLogticks'
#'
#' These options will be used on objects of class 'GuideAxisLogticks'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"guide_axis_logticks"` (default): We build the object using `ggplot2::guide_axis_logticks()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_GuideAxisLogticks>
#' @export
opts_GuideAxisLogticks <- function(constructor = c("guide_axis_logticks", "next"), ...) {
  constructive::.cstr_options("GuideAxisLogticks", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.GuideAxisLogticks <- function(x, ...) {
  opts <- list(...)$opts$GuideAxisLogticks %||% opts_GuideAxisLogticks()
  if (is_corrupted_GuideAxisLogticks(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.GuideAxisLogticks", structure(NA, class = opts$constructor))
}

is_corrupted_GuideAxisLogticks <- function(x) {
  !is.environment(x) ||
    !is.function(x$super) ||
    !identical(x$params$name, "axis")
}

#' @export
.cstr_construct.GuideAxisLogticks.guide_axis_logticks <- function(x, ...) {
  if (identical(x, ggplot2::GuideAxisLogticks)) return("ggplot2::GuideAxisLogticks")
  args <- environment(x$super)$env$args # x$params
  args$name <- NULL
  args$minor.ticks <- NULL
  args$prescale.base <- args$prescale_base
  args$prescale_base <- NULL
  args$negative.small <- args$negative_small
  args$negative_small <- NULL
  args$short.theme <- args$short_theme
  args$short_theme <- NULL
  if (inherits(args$long, "rel")) args$long <- unclass(args$long)
  if (inherits(args$mid, "rel")) args$mid <- unclass(args$mid)
  if (inherits(args$short, "rel")) args$short <- unclass(args$short)
  args <- keep_only_non_defaults(args, ggplot2::guide_axis_logticks)
  code <- constructive::.cstr_apply(args, fun = "ggplot2::guide_axis_logticks", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("GuideAxisLogticks", "GuideAxis", "Guide", "ggproto", "gg")
  )
}

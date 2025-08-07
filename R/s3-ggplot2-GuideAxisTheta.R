#' Constructive options for class 'GuideAxisTheta'
#'
#' These options will be used on objects of class 'GuideAxisTheta'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"guide_axis_theta"` (default): We build the object using `ggplot2::guide_axis_theta()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_GuideAxisTheta>
#' @export
opts_GuideAxisTheta <- function(constructor = c("guide_axis_theta", "next"), ...) {
  constructive::.cstr_options("GuideAxisTheta", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.GuideAxisTheta <- function(x, ...) {
  opts <- list(...)$opts$GuideAxisTheta %||% opts_GuideAxisTheta()
  if (is_corrupted_GuideAxisTheta(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.GuideAxisTheta", structure(NA, class = opts$constructor))
}

is_corrupted_GuideAxisTheta <- function(x) {
  !is.environment(x) ||
    !is.function(x$super) ||
    !identical(x$params$name, "axis")
}

#' @export
.cstr_construct.GuideAxisTheta.guide_axis_theta <- function(x, ...) {
  if (identical(x, ggplot2::GuideAxis)) return("ggplot2::GuideAxisTheta")
  args <- environment(x$super)$env$args # x$params
  args$name <- NULL
  args <- keep_only_non_defaults(args, ggplot2::guide_axis_theta)
  code <- constructive::.cstr_apply(args, fun = "ggplot2::guide_axis_theta", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("GuideAxisTheta", "GuideAxis", "Guide", "ggproto", "gg")
  )
}

#' Constructive options for class 'GuideAxis'
#'
#' These options will be used on objects of class 'GuideAxis'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"guide_axis"` (default): We build the object using `ggplot2::guide_axis()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_GuideAxis>
#' @export
opts_GuideAxis <- function(constructor = c("guide_axis", "next"), ...) {
  constructive::.cstr_options("GuideAxis", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.GuideAxis <- function(x, ...) {
  opts <- list(...)$opts$GuideAxis %||% opts_GuideAxis()
  if (is_corrupted_GuideAxis(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.GuideAxis", structure(NA, class = opts$constructor))
}

is_corrupted_GuideAxis <- function(x) {
  !is.environment(x) ||
    !is.function(x$super) ||
    !identical(environment(x$super)$env$args$name, "axis")
}

#' @export
.cstr_construct.GuideAxis.guide_axis <- function(x, ...) {
  if (identical(x, ggplot2::GuideAxis)) return("ggplot2::GuideAxis")
  args <- environment(x$super)$env$args # x$params
  args$name <- NULL
  args <- keep_only_non_defaults(args, ggplot2::guide_axis)
  code <- constructive::.cstr_apply(args, fun = "ggplot2::guide_axis", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("GuideAxis", "Guide", "ggproto", "gg")
  )
}

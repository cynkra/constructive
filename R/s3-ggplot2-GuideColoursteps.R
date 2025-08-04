#' Constructive options for class 'GuideColoursteps'
#'
#' These options will be used on objects of class 'GuideColoursteps'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"guide_colorsteps"` (default): We build the object using `ggplot2::guide_colorsteps()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_GuideColoursteps>
#' @export
opts_GuideColoursteps <- function(constructor = c("guide_colorsteps", "next"), ...) {
  constructive::.cstr_options("GuideColoursteps", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.GuideColoursteps <- function(x, ...) {
  opts <- list(...)$opts$GuideColoursteps %||% opts_GuideColoursteps()
  if (is_corrupted_GuideColoursteps(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.GuideColoursteps", structure(NA, class = opts$constructor))
}

is_corrupted_GuideColoursteps <- function(x) {
  !is.environment(x) ||
    !is.function(x$super) ||
    !identical(x$params$name, "colourbar")
}

#' @export
.cstr_construct.GuideColoursteps.guide_colorsteps <- function(x, ...) {
  if (identical(x, ggplot2::GuideColoursteps)) return("ggplot2::GuideColoursteps")
  args <- environment(x$super)$env$args # x$params
  args$show.limits <- x$params$show.limits
  if (!length(args$override.aes)) args$override.aes <- NULL
  args <- keep_only_non_defaults(args, ggplot2::guide_colorsteps)
  code <- constructive::.cstr_apply(args, fun = "ggplot2::guide_colorsteps", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("GuideColoursteps", "GuideColourbar", "GuideLegend", "Guide", "ggproto", "gg")
  )
}

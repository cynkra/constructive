#' Constructive options for class 'GuideNone'
#'
#' These options will be used on objects of class 'GuideNone'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"guide_none"` (default): We build the object using `ggplot2::guide_none()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_GuideNone>
#' @export
opts_GuideNone <- function(constructor = c("guide_none", "next"), ...) {
  constructive::.cstr_options("GuideNone", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.GuideNone <- function(x, ...) {
  opts <- list(...)$opts$GuideNone %||% opts_GuideNone()
  if (is_corrupted_GuideNone(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.GuideNone", structure(NA, class = opts$constructor))
}

is_corrupted_GuideNone <- function(x) {
  FALSE
}

#' @export
.cstr_construct.GuideNone.guide_none <- function(x, ...) {
  if (identical(x, ggplot2::GuideNone)) return("ggplot2::GuideNone")
  args <- environment(x$super)$env$args # x$params
  args$hash <- NULL
  if (!length(args$override.aes)) args$override.aes <- NULL
  args <- keep_only_non_defaults(args, ggplot2::guide_none)
  code <- constructive::.cstr_apply(args, fun = "ggplot2::guide_none", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("GuideNone", "Guide", "ggproto", "gg")
  )
}

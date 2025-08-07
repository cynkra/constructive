#' Constructive options for class 'GuideAxisStack'
#'
#' These options will be used on objects of class 'GuideAxisStack'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"guide_axis_stack"` (default): We build the object using `ggplot2::guide_axis_stack()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_GuideAxisStack>
#' @export
opts_GuideAxisStack <- function(constructor = c("guide_axis_stack", "next"), ...) {
  constructive::.cstr_options("GuideAxisStack", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.GuideAxisStack <- function(x, ...) {
  opts <- list(...)$opts$GuideAxisStack %||% opts_GuideAxisStack()
  if (is_corrupted_GuideAxisStack(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.GuideAxisStack", structure(NA, class = opts$constructor))
}

is_corrupted_GuideAxisStack <- function(x) {
  !is.environment(x) ||
    !is.function(x$super) ||
    !identical(x$params$name, "stacked_axis")
}

#' @export
.cstr_construct.GuideAxisStack.guide_axis_stack <- function(x, ...) {
  if (identical(x, ggplot2::GuideAxisStack)) return("ggplot2::GuideAxisStack")
  args <- c(
    first = environment(x$super)$env$pf$first,
    x$params$guides[-1]
  )
  #   as.list(environment(x$super)$env$pf)
  # args <- x$params$guides # environment(x$super)$env$args # x$params
  #args$name <- NULL
  args <- keep_only_non_defaults(args, ggplot2::guide_axis_stack)
  if (length(x$params$guides) > 1 && is.null(args$first)) {
    args <- c(first = "axis", args)
  }
  code <- constructive::.cstr_apply(args, fun = "ggplot2::guide_axis_stack", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("GuideAxisStack", "GuideAxis", "Guide", "ggproto", "gg")
  )
}

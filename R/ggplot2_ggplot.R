#' @export
construct_idiomatic.ggplot <- function(x, pipe, ...) {
  ## data
  code <- construct_raw(x$data, pipe = pipe, ...)
  ## main object
  if (length(x$mapping)) {
    mapping_code <- construct_raw(x$mapping, pipe = pipe, ...)
    ggplot_code <- construct_apply(mapping_code, fun = "ggplot2::ggplot", language = TRUE, new_line = FALSE, pipe = pipe, ...)
  } else {
    ggplot_code <- "ggplot2::ggplot()"
  }

  code <- pipe(code, ggplot_code, pipe = pipe)
  ## layers
  if (length(x$layers)) {
    layer_code1 <- lapply(x$layers, construct_raw, pipe = pipe, ...)
    layer_code <- Reduce(function(x, y)  pipe(x,y, pipe = "plus"), layer_code1)
    code <- pipe(code, layer_code, pipe = "plus")
  }
  ## facets
  if (!inherits(x$facet, "FacetNull")) {
    facet_code <- construct_raw(x$facet, pipe = pipe, ...)
    code <- pipe(code, facet_code, pipe = "plus")
  }
  ## labels
  labels <- x$labels
  mappings <- unlist(c(x$mapping, lapply(x$layers, function(x) x$mapping)))
  mappings <- mappings[unique(names(mappings))]
  default_labs <- vapply(mappings, function(x) rlang::expr_deparse(rlang::quo_squash(x)), character(1))
  default_labs <- sub(" ([+-/*]) ", "\\1", default_labs)
  common_nms <- intersect(names(labels), names(default_labs))
  to_remove <- common_nms[labels[common_nms] == default_labs[common_nms] & !is.na(labels[common_nms])]
  labels <- labels[setdiff(names(labels), to_remove)]
  if (length(labels)) {
    class(labels) <- "labels" # the class is dropped for some reason
    labs_code <- construct_raw(labels, pipe = pipe, ...)
    code <- pipe(code, labs_code, pipe = "plus")
  }
  ## scales
  if (length(x$scales$scales)) {
    #browser()
    scales_code <- construct_raw(x$scales, pipe = pipe, ScalesList.as_sum_of_scales = TRUE, ...)
    code <- pipe(code, scales_code, pipe = "plus")
  }
  ## theme
  theme <- x$theme
  if (length(theme)) {
    class(theme) <- c("theme", "gg")
    theme_code <- construct_raw(theme, pipe = pipe, ...)
    code <- pipe(code, theme_code, pipe = "plus")
  }

  code
}

#' @export
repair_attributes.ggplot <- function(x, code, pipe = "base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    idiomatic_class = c("gg", "ggplot"),
    ...
  )
}

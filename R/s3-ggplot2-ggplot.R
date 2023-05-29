#' @export
.cstr_construct.ggplot <- function(x, ...) {
  ## ggplot call
  code <- construct_ggplot_call(x$mapping, ...)

  ## data
  # as a second step because sometimes we have no data
  code <- pipe_from_data(x$data, code, ...)

  ## layers
  code <- pipe_to_layers(code, x$layers, plot_env = x$plot_env, ...)

  ## facets
  code <- pipe_to_facets(code, x$facet, ...)

  ## labels
  code <- pipe_to_labels(code, x$labels, x$mapping, x$layers, ...)

  ## scales
  code <- pipe_to_scales(code, x$scales, ...)

  ## theme
  code <- pipe_to_theme(code, x$theme, ...)

  ## coord
  code <- pipe_to_coord(code, x$coordinates, ...)

  repair_attributes_ggplot(x, code, ...)
}

repair_attributes_ggplot <- function(x, code, pipe = "base", ...) {
  .cstr_repair_attributes(
    x, code, pipe,
    idiomatic_class = c("gg", "ggplot"),
    ...
  )
}

construct_ggplot_call <- function(mapping, ...) {
  if (!length(mapping)) return("ggplot2::ggplot()")
  mapping_code <- .cstr_construct(mapping, ...)
  .cstr_apply(mapping_code, fun = "ggplot2::ggplot", recurse = FALSE, new_line = FALSE, ...)
}

pipe_from_data <- function(plot_data, code, ..., pipe, one_liner) {
  if (!length(plot_data)) return(code)
  data_code <- .cstr_construct(plot_data, pipe = pipe, one_liner = one_liner, ...)
  .cstr_pipe(data_code, code, pipe = pipe, one_liner = one_liner)
}

pipe_to_layers <- function(code, layers, plot_env, ..., one_liner, env) {
  if (!length(layers)) return(code)
  layer_lines <- lapply(layers, .cstr_construct, one_liner = one_liner, env = plot_env, ...)
  layer_code <- Reduce(function(x, y)  .cstr_pipe(x, y, pipe = "plus", one_liner = one_liner), layer_lines)
  .cstr_pipe(code, layer_code, pipe = "plus", one_liner = one_liner)
}

pipe_to_facets <- function(code, facet, ..., one_liner) {
  if (inherits(facet, "FacetNull")) return(code)
  facet_code <- .cstr_construct(facet, one_liner = one_liner, ...)
  .cstr_pipe(code, facet_code, pipe = "plus", one_liner = one_liner)
}

pipe_to_labels <- function(code, labels, mapping, layers, ..., one_liner) {
  # discard default labels  tagged as "fallback"
  labels <- Filter(function(x) !isTRUE(attr(x, "fallback")), labels)
  # FIXME: some tests around labels to make sure they behave ok, need to try some
  # ggtitle and some combinations of NULL and waiver() inputs for x,y and others
  # if x and y are not found first it means they were not provided, and we should remove them
  # we cannot just remove them if they're NULL
  if (names(labels)[1] == "x") {
    if (names(labels)[2] != "y") labels[["y"]] <- NULL
  } else {
    labels[["x"]] <- NULL
    if (names(labels)[1] != "y") labels[["y"]] <- NULL
  }
  mappings <- unlist(c(mapping, lapply(layers, function(x) x$mapping)))
  mappings <- mappings[unique(names(mappings))]
  # discard mappings given as syntactic literals
  litts <- vapply(mappings, rlang::is_syntactic_literal, logical(1))
  litt_nms <- names(mappings[litts])
  mappings <- mappings[!litts]
  default_labs <- vapply(
    mappings,
    function(x) {
      x <- rlang::quo_squash(x)
      if (rlang::is_call(x, "after_stat")) x <- x[[2]]
      rlang::expr_deparse(x)
    },
    character(1)
  )
  # remove space between around "/", for some reason ggplot seems to do this for this op only
  default_labs <- sub(" ([/]) ", "\\1", default_labs)
  common_nms <- intersect(names(labels), names(default_labs))
  to_remove <- c(
    common_nms[labels[common_nms] == default_labs[common_nms] & !is.na(labels[common_nms])],
    litt_nms
  )
  labels <- labels[setdiff(names(labels), to_remove)]
  if (length(labels)) {
    class(labels) <- "labels" # the class is dropped for some reason
    labs_code <- .cstr_construct(labels, one_liner = one_liner, ...)
    code <- .cstr_pipe(code, labs_code, pipe = "plus", one_liner = one_liner)
  }
  code
}

pipe_to_scales <- function(code, scales, ..., one_liner) {
  if (!length(scales$scales)) return(code)
  scales_code <- .cstr_construct(scales, one_liner = one_liner, ...)
  .cstr_pipe(code, scales_code, pipe = "plus", one_liner = one_liner)
}

pipe_to_theme <- function(code, theme, ..., one_liner) {
  # an empty theme has attributes "complete" and "validate" it has a (non functional) effect
  if (!length(theme) && !length(attributes(theme))) return(code)
  class(theme) <- c("theme", "gg")
  theme_code <- .cstr_construct(theme, one_liner = one_liner, ...)
  .cstr_pipe(code, theme_code, pipe = "plus", one_liner = one_liner)
}

pipe_to_coord <- function(code, coord, ..., one_liner) {
  coord_code <- .cstr_construct(coord, one_liner = one_liner, ...)
  if (identical(coord_code, "ggplot2::coord_cartesian()")) return(code)
  .cstr_pipe(code, coord_code, pipe = "plus", one_liner = one_liner)
}

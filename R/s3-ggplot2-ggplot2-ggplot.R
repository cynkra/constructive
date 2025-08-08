#' Constructive options for class 'ggplot2::ggplot'
#'
#' These options will be used on objects of class 'ggplot2::ggplot'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"ggplot"` (default): Use `ggplot2::ggplot()`
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"list"` : Use `list()` and treat the class as a regular attribute.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_ggplot2::ggplot>
#' @export
opts_ggplot2_ggplot <- function(constructor = c("ggplot", "next", "list"), ...) {
  .cstr_options("ggplot2::ggplot", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ggplot2::ggplot
`.cstr_construct.ggplot2::ggplot` <- function(x, ...) {
  opts <- list(...)$opts$`ggplot2::ggplot` %||% opts_ggplot2_ggplot()
  if (`is_corrupted_ggplot2::ggplot`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ggplot2::ggplot", structure(NA, class = opts$constructor))
}

`is_corrupted_ggplot2::ggplot` <- function(x) {
  typeof(x) != "object"
}

#' @export
#' @method .cstr_construct.ggplot2::ggplot list
`.cstr_construct.ggplot2::ggplot.list` <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
#' @method .cstr_construct.ggplot2::ggplot ggplot
`.cstr_construct.ggplot2::ggplot.ggplot` <- function(x, ...) {
  ## ggplot call
  code <- construct_ggplot_call(x@mapping, ...)

  ## data
  # as a second step because sometimes we have no data
  code <- pipe_from_data(x@data, code, ...)

  ## layers
  code <- pipe_to_layers(code, x@layers, plot_env = x@plot_env, ...)

  ## facets
  code <- pipe_to_facets(code, x@facet, ...)

  ## labels
  code <- pipe_to_labels(code, x@labels, x@mapping, x@layers, ...)

  ## scales
  code <- pipe_to_scales(code, x@scales, ...)

  ## theme
  code <- pipe_to_theme(code, x@theme, ...)

  ## coord
  code <- pipe_to_coord(code, x@coordinates, ...)

  ## coord
  code <- pipe_to_guide(code, x@guides, ...)

  `repair_attributes_ggplot2::ggplot`(x, code, ...)
}

`repair_attributes_ggplot2::ggplot` <- function(x, code, pipe = NULL, one_liner, ...) {
  if (one_liner) {
    code_with_parens <- paste0("(", code, ")")
  } else {
    code_with_parens <- c("(", indent(code), ")")
  }

  code_with_attrs <- .cstr_repair_attributes(
    x, code_with_parens, pipe = pipe,
    idiomatic_class = c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg"),
    ignore = c(
      "S7_class", "data", "layers", "scales", "guides", "mapping", "theme",
      "coordinates", "facet", "layout", "labels", "meta", "plot_env"
    ),
    one_liner = one_liner,
    ...
  )
  nothing_to_repair <- identical(code_with_attrs, code_with_parens)
  if (nothing_to_repair) return(code)
  code_with_attrs
}

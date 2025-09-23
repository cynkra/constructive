#' Constructive options for class 'CoordTransform'
#'
#' These options will be used on objects of class 'CoordTransform'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"coord_transform"` (default): We build the object using `ggplot2::coord_transform()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_CoordTransform>
#' @export
opts_CoordTransform <- function(constructor = c("coord_transform", "next"), ...) {
  constructive::.cstr_options("CoordTransform", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.CoordTransform <- function(x, ...) {
  opts <- list(...)$opts$CoordTransform %||% opts_CoordTransform()
  if (is_corrupted_CoordTransform(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.CoordTransform", structure(NA, class = opts$constructor))
}

is_corrupted_CoordTransform <- function(x) {
  if (!is.environment(x)) return(TRUE)
  if (!all(c("expand", "clip", "limits", "trans", "reverse", "super") %in% names(x))) return(TRUE)
  if (!is.list(x$trans) || !all(c("x", "y") %in% names(x$trans))) return(TRUE)
  if (!is.list(x$trans$x) || !is.list(x$trans$y)) return(TRUE)
  if (!is.list(x$limits) || !all(c("x", "y") %in% names(x$limits))) return(TRUE)
  if (!rlang::is_string(x$clip)) return(TRUE)
  if (!rlang::is_bool(x$expand)) return(TRUE)
  if (!rlang::is_string(x$reverse)) return(TRUE)
  FALSE
}

#' @export
.cstr_construct.CoordTransform.coord_transform <- function(x, ...) {
  # opts <- list(...)$opts$CoordTransform %||% opts_CoordTransform()
  args <- list(
    x = x$trans$x$name,
    y = x$trans$y$name,
    xlim = x$limits$x,
    ylim = x$limits$y,
    clip = x$clip,
    expand = x$expand,
    reverse = x$reverse
  )

  args <- keep_only_non_defaults(args, getFromNamespace("coord_transform", "ggplot2"))
  code <- .cstr_apply(args, "ggplot2::coord_transform", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("CoordTransform", "Coord", "ggproto", "gg")
  )
}

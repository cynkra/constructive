#' Constructive options for class 'CoordRadial'
#'
#' These options will be used on objects of class 'CoordRadial'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"coord_radial"` (default): We build the object using `ggplot2::coord_radial()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_CoordRadial>
#' @export
opts_CoordRadial <- function(constructor = c("coord_radial", "next"), ...) {
  constructive::.cstr_options("CoordRadial", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.CoordRadial <- function(x, ...) {
  opts <- list(...)$opts$CoordRadial %||% opts_CoordRadial()
  if (is_corrupted_CoordRadial(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.CoordRadial", structure(NA, class = opts$constructor))
}

is_corrupted_CoordRadial <- function(x) {
  FALSE
}

#' @export
.cstr_construct.CoordRadial.coord_radial <- function(x, ...) {


  arc <- switch(x$reverse,
                thetar = rev(x$arc),
                theta = rev(x$arc),
                x$arc)
  start <- arc[1]
  end <- arc[2]

  if (start > end) {
    n_rotate <- ((start - end) %/% (2 * pi)) + 1
    start <- start + n_rotate * 2 * pi
  }
  if (end == start + 2 * pi) {
    end <- NULL
  }

  args <- list(
    theta = x$theta,
    start = start,
    end = end,
    thetalim = x$limits$theta,
    rlim = x$limits$r,
    expand = x$expand,
    clip = x$clip,
    r.axis.inside = x$r_axis_inside,
    rotate.angle = x$rotate_angle,
    inner.radius = x$inner_radius[[1]] / 0.4,
    reverse = x$reverse
  )
  if (is.null(end) && isFALSE(args$r.axis.inside)) {
    args$r.axis.inside <- NULL
  } else  if (!is.null(end) && isTRUE(args$r.axis.inside)) {
    args$r.axis.inside <- NULL
  }
  args <- keep_only_non_defaults(args, ggplot2::coord_radial)
  code <- constructive::.cstr_apply(args, fun = "ggplot2::coord_radial", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("CoordRadial", "Coord", "ggproto", "gg")
  )
}

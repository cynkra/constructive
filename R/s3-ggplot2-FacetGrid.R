#' @export
#' @rdname other-opts
opts_FacetGrid <- function(constructor = c("facet_grid", "ggproto", "next", "environment"), ...) {
  .cstr_options("FacetGrid", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct FacetGrid
.cstr_construct.FacetGrid <- function(x, ...) {
  opts <- list(...)$opts$FacetGrid %||% opts_FacetGrid()
  if (is_corrupted_FacetGrid(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.FacetGrid", structure(NA, class = opts$constructor))
}

is_corrupted_FacetGrid <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.FacetGrid environment
.cstr_construct.FacetGrid.environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
#' @method .cstr_construct.FacetGrid ggproto
.cstr_construct.FacetGrid.ggproto <- function(x, ...) {
  .cstr_construct.ggproto(x, ...)
}

#' @export
#' @method .cstr_construct.FacetGrid facet_grid
.cstr_construct.FacetGrid.facet_grid <- function(x, ...) {
  args <- as.list(x)

  scales_ind <-  unlist(x$params$free) + 1
  scales <- list(list("fixed", "free_y"), list("free_x", "free"))[[scales_ind]]

  space_ind <-  unlist(x$params$space_free) + 1
  space <- list(list("fixed", "free_y"), list("free_x", "free"))[[space_ind]]


  if (inherits(environment(x$super)$env$rows, "formula")) {
    rows <- .cstr_construct(environment(x$super)$env$rows, ...)
    cols <- NULL
  } else {
    # x$params$rows and x$params$cols are of class "quosures" "list"
    # so we cannot use a specific constructor and need to handle it adhoc here
    rows <- if (length(x$params$rows)) {
      rows <- unname(vapply(
        x$params$rows,
        function(x) rlang::expr_deparse(rlang::quo_squash(x)),
        character(1)
      ))
      .cstr_apply(rows, "ggplot2::vars", recurse = FALSE, new_line = FALSE, ...)
    }

    cols <- if (length(x$params$cols)) {
      cols <- unname(vapply(
        x$params$cols,
        function(x) rlang::expr_deparse(rlang::quo_squash(x)),
        character(1)
      ))
      .cstr_apply(cols, "ggplot2::vars", recurse = FALSE, new_line = FALSE, ...)
    }
  }


  # the axes parameter maps to a combination of draw_axes$x and draw_axes$y
  if (!x$params$draw_axes$x && !x$params$draw_axes$y) {
    axes <- "margins"
  } else if (x$params$draw_axes$x && x$params$draw_axes$y) {
    axes <- "all"
  } else if (x$params$draw_axes$x) {
    axes <- "all_x"
  } else {
    axes <- "all_y"
  }

  # the axis.labels parameter maps to a combination of axis_labels$x and axis_labels$y
  if (!x$params$axis_labels$x && !x$params$axis_labels$y) {
    axis.labels <- "margins"
  } else if (x$params$axis_labels$x && x$params$axis_labels$y) {
    axis.labels <- "all"
  } else if (x$params$axis_labels$x) {
    axis.labels <- "all_x"
  } else {
    axis.labels <- "all_y"
  }

  args <- list(
    rows = x$params$rows,
    cols = x$params$cols,
    scales = scales,
    space = space,
    shrink = x$shrink, # not under params!
    labeller = x$params$labeller,
    as.table = x$params$as.table, # NULL for new versions
    # switch is deprecated
    # switch =,
    drop = x$params$drop,
    margins = x$params$margins,
    axes = axes,
    axis.labels = axis.labels
  )

  # remove if same as default
  if (isTRUE(args$scales == "fixed")) args$scales <- NULL
  if (isTRUE(args$space == "fixed")) args$space <- NULL
  if (isTRUE(args$shrink)) args$shrink <- NULL
  if (identical(args$labeller, getFromNamespace("label_value", "ggplot2")) || identical(args$labeller, "label_value")) args$labeller <- NULL
  if (is.null(args$switch)) args$switch <- NULL
  if (isTRUE(args$drop)) args$drop <- NULL
  if (isFALSE(args$margins)) args$margins <- NULL
  if (isTRUE(args$axes == "margins")) args$axes <- NULL
  if (isTRUE(args$axis.labels == "all")) args$axis.labels <- NULL

  # In 3.5.1 and after 3.5.2 (not included) as.table is not stored
  # It seems the old behavior was temporarily switched back for 3.5.2
  if (with_versions(ggplot2 != "3.5.1" && ggplot2 < "3.5.2.9000")) {
    if (isTRUE(args$as.table)) args$as.table <- NULL
    if (isTRUE(args$dir == "h")) args$dir <- NULL
  } else {
    if (isTRUE(args$dir == "lt")) {
      args$as.table <- NULL # TRUE by default
      args$dir <- NULL
    } else if (isTRUE(args$dir == "lb")) {
      args$as.table <- FALSE
      args$dir <- NULL
    } else {
      args$as.table <- NULL
    }
  }

  ## build call
  args <- lapply(args, function(x, ...) .cstr_construct(x, ...), ...)
  args$rows <- rows
  args$cols <- cols
  code <- .cstr_apply(args, fun = "ggplot2::facet_grid", recurse = FALSE, ...)
  repair_attributes_FacetGrid(x, code, ...)
}

repair_attributes_FacetGrid <- function(x, code, pipe = NULL, ...) {
  code
}

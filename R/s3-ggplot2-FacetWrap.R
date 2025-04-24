#' @export
#' @rdname other-opts
opts_FacetWrap <- function(constructor = c("facet_wrap", "ggproto", "next", "environment"), ...) {
  .cstr_options("FacetWrap", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct FacetWrap
.cstr_construct.FacetWrap <- function(x, ...) {
  opts <- list(...)$opts$FacetWrap %||% opts_FacetWrap()
  if (is_corrupted_FacetWrap(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.FacetWrap", structure(NA, class = opts$constructor))
}

is_corrupted_FacetWrap <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.FacetWrap environment
.cstr_construct.FacetWrap.environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
#' @method .cstr_construct.FacetWrap ggproto
.cstr_construct.FacetWrap.ggproto <- function(x, ...) {
  .cstr_construct.ggproto(x, ...)
}

#' @export
#' @method .cstr_construct.FacetWrap facet_wrap
.cstr_construct.FacetWrap.facet_wrap <- function(x, ...) {
  args <- as.list(x)

  scales_ind <-  unlist(x$params$free) + 1
  scales <- list(list("fixed", "free_y"), list("free_x", "free"))[[scales_ind]]

  # x$params$facets is of class "quosures" "list"
  # so we cannot use a specific constructor and need to handle it adhoc here
  facets <- unname(vapply(
    x$params$facets,
    function(x) rlang::expr_deparse(rlang::quo_squash(x)),
    character(1)
    ))
  facets <- .cstr_apply(facets, "ggplot2::vars", recurse = FALSE, new_line = FALSE, ...)

  args <- list(
    facets = NULL,
    nrow = x$params$nrow,
    ncol = x$params$ncol,
    scales = scales,
    shrink = x$shrink,
    labeller = x$params$labeller,
    as.table = x$params$as.table, # NULL for new versions
    # switch is deprecated
    # switch =,
    drop = x$params$drop,
    dir = x$params$dir,
    strip.position = x$params$strip.position
  )

  # remove if same as default
  if (is.null(args$nrow)) args$nrow <- NULL
  if (is.null(args$ncol)) args$ncol <- NULL
  if (isTRUE(args$scales == "fixed")) args$scales <- NULL
  if (isTRUE(args$shrink)) args$shrink <- NULL
  if (identical(args$labeller, getFromNamespace("label_value", "ggplot2")) || identical(args$labeller, "label_value")) args$labeller <- NULL
  if (is.null(args$switch)) args$switch <- NULL
  if (isTRUE(args$drop)) args$drop <- NULL
  if (isTRUE(args$strip.position == "top")) args$strip.position <- NULL

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
  args$facets <- facets
  code <- .cstr_apply(args, fun = "ggplot2::facet_wrap", recurse = FALSE, ...)
  repair_attributes_FacetWrap(x, code, ...)
}

repair_attributes_FacetWrap <- function(x, code, pipe = NULL, ...) {
  code
}

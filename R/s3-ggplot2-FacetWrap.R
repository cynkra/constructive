#' @export
.cstr_construct.FacetWrap <- function(x, ...) {
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
    as.table = x$params$as.table,
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
  if (identical(args$labeller, quote(label_value)) || identical(args$labeller, "label_value")) args$labeller <- NULL
  if (isTRUE(args$as.table)) args$as.table <- NULL
  if (is.null(args$switch)) args$switch <- NULL
  if (isTRUE(args$drop)) args$drop <- NULL
  if (isTRUE(args$dir == "h")) args$dir <- NULL
  if (isTRUE(args$strip.position == "top")) args$strip.position <- NULL

  ## build call
  args <- lapply(args, .cstr_construct, ...)
  args$facets <- facets
  .cstr_apply(args, fun = "ggplot2::facet_wrap", recurse = FALSE, ...)
}

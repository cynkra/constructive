#' Constructive options for class 'shiny.tag'
#'
#' These options will be used on objects of class 'shiny.tag'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"tags"` (default): We build the object using `htmltools::tags$<name>()`,
#'   e.g. `htmltools::tags$div()`, with attributes as named arguments and children as
#'   unnamed arguments. If the tag name is not a known tag or if some attributes
#'   would clash with the formal arguments of the tag function, we fall back to
#'   the `"tag"` constructor.
#' * `"tag"` : We build the object using `htmltools::tag()`. `htmltools::tag()`
#'   can't create an empty named list of attributes (as `htmltools::tags$div()`
#'   does), in this case we fall back to the `"tags"` constructor for known tag
#'   names, and to the list constructor otherwise.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_shiny.tag>
#' @export
opts_shiny.tag <- function(constructor = c("tags", "tag", "next"), ...) {
  .cstr_options("shiny.tag", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct shiny.tag
.cstr_construct.shiny.tag <- function(x, ...) {
  opts <- list(...)$opts$shiny.tag %||% opts_shiny.tag()
  if (is_corrupted_shiny.tag(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.shiny.tag", structure(NA, class = opts$constructor))
}

is_corrupted_shiny.tag <- function(x) {
  if (!is.list(x)) return(TRUE)
  nms <- names(x)
  expected <- c("name", "attribs", "children", ".noWS", ".renderHooks")
  if (!identical(nms, intersect(expected, nms)) || !all(expected[1:3] %in% nms)) {
    return(TRUE)
  }
  if (!rlang::is_string(x$name)) return(TRUE)
  attribs <- x$attribs
  attribs_nms <- names(attribs)
  # `htmltools::tag()` drops NULL and empty attributes
  if (
    !is.list(attribs) ||
    !all(names(attributes(attribs)) == "names") ||
    (length(attribs) && (anyNA(attribs_nms) || !all(nzchar(attribs_nms)))) ||
    any(lengths(attribs) == 0)
  ) {
    return(TRUE)
  }
  if (!is.list(x$children) || !is.null(attributes(x$children))) return(TRUE)
  if (".noWS" %in% nms && !is_valid_no_ws(x$.noWS)) return(TRUE)
  ".renderHooks" %in% nms && !is.list(x$.renderHooks)
}

#' @export
#' @method .cstr_construct.shiny.tag tags
.cstr_construct.shiny.tag.tags <- function(x, ...) {
  attribs <- x$attribs
  known_tag <- x$name %in% names(htmltools::tags)
  # `htmltools::tags$<name>()` forwards its dots to `rlang::dots_list()`
  reserved_nms <- setdiff(
    c(names(formals(htmltools::tags$div)), names(formals(rlang::dots_list))),
    "..."
  )
  if (!known_tag || is.null(names(attribs)) || any(names(attribs) %in% reserved_nms)) {
    return(.cstr_construct.shiny.tag.tag(x, ...))
  }
  args <- c(attribs, x$children, shiny_tag_extra_args(x))
  code <- .cstr_apply(args, fun = paste0("htmltools::tags$", protect(x$name)), ...)
  repair_attributes_shiny.tag(x, code, ...)
}

#' @export
#' @method .cstr_construct.shiny.tag tag
.cstr_construct.shiny.tag.tag <- function(x, ...) {
  attribs <- x$attribs
  # an empty named `attribs` can't be produced by `htmltools::tag()`
  if (!length(attribs) && !is.null(names(attribs))) {
    if (x$name %in% names(htmltools::tags)) return(.cstr_construct.shiny.tag.tags(x, ...))
    return(.cstr_construct.list(x, ...))
  }
  args <- c(list(x$name, c(attribs, x$children)), shiny_tag_extra_args(x))
  code <- .cstr_apply(args, fun = "htmltools::tag", ...)
  repair_attributes_shiny.tag(x, code, ...)
}

shiny_tag_extra_args <- function(x) {
  args <- list()
  args$.noWS <- x$.noWS
  hooks <- x$.renderHooks
  # `htmltools::tag()` wraps a single hook into a list
  if (is.null(attributes(hooks)) && length(hooks) == 1 && !is.list(hooks[[1]])) {
    hooks <- hooks[[1]]
  }
  args$.renderHook <- hooks
  args
}

is_valid_no_ws <- function(x) {
  no_ws_options <- c("before", "after", "after-begin", "before-end", "outside", "inside")
  is.character(x) && all(x %in% no_ws_options)
}

repair_attributes_shiny.tag <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "shiny.tag"
  )
}

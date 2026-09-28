#' Constructive options for class 'dist'
#'
#' These options will be used on objects of class 'dist'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as.dist"` (default): We build the object using `as.dist()` on a symmetric
#'   matrix. The "call" attribute, and the "method" attribute set by `dist()`,
#'   are then repaired.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `opts_atomic()`.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_dist>
#' @export
opts_dist <- function(constructor = c("as.dist", "next"), ...) {
  .cstr_options("dist", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct dist
.cstr_construct.dist <- function(x, ...) {
  opts <- list(...)$opts$dist %||% opts_dist()
  if (is_corrupted_dist(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.dist", structure(NA, class = opts$constructor))
}

is_corrupted_dist <- function(x) {
  size <- attr(x, "Size")
  labels <- attr(x, "Labels")
  is_flag <- function(x) is.logical(x) && length(x) == 1 && !is.na(x)
  !is.double(x) ||
    !is.integer(size) ||
    length(size) != 1 ||
    is.na(size) ||
    length(x) != size * (size - 1) / 2 ||
    !is.null(labels) && (!is.character(labels) || length(labels) != size) ||
    !is_flag(attr(x, "Diag")) ||
    !is_flag(attr(x, "Upper"))
}

#' @export
#' @method .cstr_construct.dist as.dist
.cstr_construct.dist.as.dist <- function(x, ...) {
  size <- attr(x, "Size")
  labels <- attr(x, "Labels")
  x_stripped <- x
  attributes(x_stripped) <- NULL
  m <- matrix(0, size, size)
  m[lower.tri(m)] <- x_stripped
  m <- m + t(m)
  if (!is.null(labels)) attr(m, "dimnames") <- list(labels, labels)
  args <- list(m)
  if (attr(x, "Diag")) args$diag <- TRUE
  if (attr(x, "Upper")) args$upper <- TRUE
  code <- .cstr_apply(args, "as.dist", ...)
  repair_attributes_dist(x, code, ...)
}

repair_attributes_dist <- function(x, code, ...) {
  # as.dist() sets a "call" attribute that we cannot predict, we always repair it
  remove <- if (is.null(attr(x, "call"))) "call"
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "dist",
    ignore = c("Size", "Labels", "Diag", "Upper"),
    remove = remove
  )
}

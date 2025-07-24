#' @export
#' @rdname other-opts
opts_margin <- function(constructor = c("margin", "next", "double"), ...) {
  .cstr_options("margin", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct margin
.cstr_construct.margin <- function(x, ...) {
  opts <- list(...)$opts$margin %||% opts_margin()
  if (is_corrupted_margin(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.margin", structure(NA, class = opts$constructor))
}

is_corrupted_margin <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.margin double
.cstr_construct.margin.double <- function(x, ...) {
  .cstr_construct.double(x, ...)
}

#' @export
#' @method .cstr_construct.margin margin
.cstr_construct.margin.margin <- function(x, ...) {
  lkp <- c(
    npc = 0L, cm = 1L, inches = 2L, mm = 7L, points = 8L, picas = 9L,
    bigpts = 10L, dida = 11L, cicero = 12L, scaledpts = 13L, lines = 3L,
    char = 18L, native = 4L, snpc = 6L, mylines = 103L, mychar = 104L,
    mystrwidth = 105L, mystrheight = 106L
  )
  unit <- names(lkp)[match(attr(x, "unit"), lkp)]
  x0 <- as.vector(x)
  code <- .cstr_apply(list(t = x0[1], r = x0[2], b = x0[3], l = x0[4], unit = unit), "ggplot2::margin", ...)
  repair_attributes_margin(x, code, ...)
}

repair_attributes_margin <- function(x, code, ...) {
  .cstr_repair_attributes(x, code, idiomatic_class = c("margin", "simpleUnit", "unit", "unit_v2"), ignore = "unit", ...)
}

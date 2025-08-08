#' @export
#' @rdname other-opts
opts_ggplot2_margin <- function(constructor = c("margin", "next", "double"), ...) {
  .cstr_options("ggplot2::margin", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ggplot2::margin
`.cstr_construct.ggplot2::margin` <- function(x, ...) {
  opts <- list(...)$opts$`ggplot2::margin` %||% opts_ggplot2_margin()
  if (`is_corrupted_ggplot2::margin`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ggplot2::margin", structure(NA, class = opts$constructor))
}

`is_corrupted_ggplot2::margin` <- function(x) {
  !is.double(x) && !is.null(attr(x, "S7_class"))
}

#' @export
#' @method .cstr_construct.ggplot2::margin double
`.cstr_construct.ggplot2::margin.double` <- function(x, ...) {
  .cstr_construct.double(x, ...)
}

#' @export
#' @method .cstr_construct.ggplot2::margin margin
`.cstr_construct.ggplot2::margin.margin` <- function(x, ...) {
  lkp <- c(
    npc = 0L, cm = 1L, inches = 2L, mm = 7L, points = 8L, picas = 9L,
    bigpts = 10L, dida = 11L, cicero = 12L, scaledpts = 13L, lines = 3L,
    char = 18L, native = 4L, snpc = 6L, mylines = 103L, mychar = 104L,
    mystrwidth = 105L, mystrheight = 106L
  )
  unit <- names(lkp)[match(attr(x, "unit"), lkp)]
  x0 <- as.vector(x)
  code <- .cstr_apply(list(t = x0[1], r = x0[2], b = x0[3], l = x0[4], unit = unit), "ggplot2::margin", ...)
  `repair_attributes_ggplot2::margin`(x, code, ...)
}

`repair_attributes_ggplot2::margin` <- function(x, code, ...) {
  .cstr_repair_attributes(
    x,
    code,
    idiomatic_class = c("ggplot2::margin", "simpleUnit", "unit", "unit_v2", "S7_object"),
    ignore = c("S7_class", "unit"),
    ...
  )
}

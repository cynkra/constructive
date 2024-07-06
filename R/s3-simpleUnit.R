#' @export
#' @rdname other-opts
opts_simpleUnit <- function(constructor = c("unit", "next", "double"), ...) {
  .cstr_options("simpleUnit", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct simpleUnit
.cstr_construct.simpleUnit <- function(x, ...) {
  opts <- list(...)$opts$simpleUnit %||% opts_simpleUnit()
  if (is_corrupted_simpleUnit(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.simpleUnit", structure(NA, class = opts$constructor))
}

is_corrupted_simpleUnit <- function(x) {
  if (!is.double(x)) return(TRUE)
  valid_units <- c(0:4, 6:13, 18L, 103:106)
  unit <- attr(x, "unit")
  is.null(unit) || !all(unit %in% valid_units)
}

#' @export
#' @method .cstr_construct.simpleUnit double
.cstr_construct.simpleUnit.double <- function(x, ...) {
  .cstr_construct.double(x, ...)
}

#' @export
#' @method .cstr_construct.simpleUnit unit
.cstr_construct.simpleUnit.unit <- function(x, ...) {
  lkp <- c(
    npc = 0L, cm = 1L, inches = 2L, mm = 7L, points = 8L, picas = 9L,
    bigpts = 10L, dida = 11L, cicero = 12L, scaledpts = 13L, lines = 3L,
    char = 18L, native = 4L, snpc = 6L, mylines = 103L, mychar = 104L,
    mystrwidth = 105L, mystrheight = 106L
  )
  units <- names(lkp)[match(attr(x, "unit"), lkp)]
  code <- .cstr_apply(list(as.vector(x), units = units), "grid::unit", ...)
  repair_attributes_simpleUnit(x, code, ...)
}

repair_attributes_simpleUnit <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("simpleUnit", "unit", "unit_v2"), ignore = "unit", ...)
}

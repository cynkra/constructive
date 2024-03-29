constructors$simpleUnit <- new.env()

#' @export
#' @rdname other-opts
opts_simpleUnit <- new_constructive_opts_function("simpleUnit", c("unit", "next", "atomic"))

#' @export
.cstr_construct.simpleUnit <- new_constructive_method("simpleUnit", c("unit", "next", "atomic"))

is_corrupted_simpleUnit <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$simpleUnit$atomic <- function(x, ...) {
  .cstr_construct.atomic(x, ...)
}

#' @export
constructors$simpleUnit$unit <- function(x, ...) {
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

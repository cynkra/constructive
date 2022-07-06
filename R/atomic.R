#' @export
construct_idiomatic.Date <- function(x, ...) {
  construct_apply(list(format(x)), "as.Date", new_line = FALSE, ...)
}

#' @export
repair_attributes.Date <- function(x, code, pipe = "base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    idiomatic_class = "Date",
    ...
  )
}

construct_idiomatic.factor <- function(x, ...) {
  default_levs <- sort(unique(as.character(x)))
  levs <- levels(x)
  args <- if (identical(default_levs, levs)) {
      construct_apply(list(levs[x]), "factor", new_line =  FALSE, ...)
    } else {
      construct_apply(list(levs[x], levels = levs), "factor", ...)
    }

}

#' @export
repair_attributes.factor <- function(x, code, pipe = "base") {
  repair_attributes_impl(
    x, code, pipe,
    ignore = "levels",
    idiomatic_class = "factor"
  )
}

#' @export
construct_idiomatic.ordered <- function(x, ...) {
  default_levs <- sort(unique(as.character(x)))
  levs <- levels(x)
  args <- if (identical(default_levs, levs)) list(levs[x]) else list(levs[x], levels = levs)
  construct_apply(args, "ordered", ...)
}

#' @export
repair_attributes.ordered <- function(x, code, pipe = "base") {
  repair_attributes_impl(
    x, code, pipe,
    ignore = "levels",
    idiomatic_class = c("ordered", "factor")
  )
}



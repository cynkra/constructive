#' Constructive options for pairlists
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"pairlist"` (default): Build the object using a `pairlist()` call.
#' * `"pairlist2"` : Build the object using a `rlang::pairlist2()` call.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_pairlist>
#' @export
opts_pairlist <- function(constructor = c("pairlist", "pairlist2"), ...) {
  .cstr_options("pairlist", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct pairlist
.cstr_construct.pairlist <- function(x, ...) {
  opts <- list(...)$opts$pairlist %||% opts_pairlist()
  if (is_corrupted_pairlist(x)) return(NextMethod())
  UseMethod(".cstr_construct.pairlist", structure(NA, class = opts$constructor))
}

is_corrupted_pairlist <- function(x) {
  typeof(x) != "pairlist"
}

#' @export
#' @method .cstr_construct.pairlist pairlist
.cstr_construct.pairlist.pairlist <- function(x, ...) {
  code <- .cstr_apply(x, "pairlist", ...)
  repair_attributes_pairlist(x, code, ...)
}

#' @export
#' @method .cstr_construct.pairlist pairlist2
.cstr_construct.pairlist.pairlist2 <- function(x, ...) {
  code <- .cstr_apply(x, "rlang::pairlist2", ...)
  repair_attributes_pairlist(x, code, ...)
}

repair_attributes_pairlist <- function(x, code, ...) {
  # FIXME: is there something to repair ?
  code
}

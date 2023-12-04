constructors$pairlist <- new.env()

#' Constructive options for pairlists
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"pairlist"` (default): Build the object using a `pairlist()` call.
#' * `"pairlist2"` : Build the object using a `rlang::pairlist2()` call.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_factor>
#' @export
opts_pairlist <- function(constructor = c("pairlist", "pairlist2"), ...) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "pairlist"),
    check_dots_empty()
  )
  .cstr_options("pairlist", constructor = constructor)
}

#' @export
.cstr_construct.pairlist <- function(x, ...) {
  opts <- .cstr_fetch_opts("pairlist", ...)
  if (is_corrupted_pairlist(x)) return(NextMethod())
  constructor <- constructors$pairlist[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_pairlist <- function(x) {
  typeof(x) != "pairlist"
}

constructors$pairlist$pairlist <- function(x, ...) {
  code <- .cstr_apply(x, "pairlist", ...)
  repair_attributes_pairlist(x, code, ...)
}

constructors$pairlist$pairlist2 <- function(x, ...) {
  code <- .cstr_apply(x, "rlang::pairlist2", ...)
  repair_attributes_pairlist(x, code, ...)
}

repair_attributes_pairlist <- function(x, code, ...) {
  # FIXME: is there something to repair ?
  code
}

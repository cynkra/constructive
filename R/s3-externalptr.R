constructors$externalptr <- new.env()

#' Constructive options for type 'externalptr'
#'
#' These options will be used on objects of type 'externalptr'. By default this
#' function is useless as nothing can be set, this is provided in case users wan
#' to extend the method with other constructors.
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"default"` : We use a special function from the constructive
#'
#' @param constructor String. Name of the function used to construct the environment.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_externalptr  <- function(constructor = c("default"), ...) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "externalptr"),
    ellipsis::check_dots_empty()
  )
  .cstr_options("externalptr", constructor = constructor)
}

#' @export
.cstr_construct.externalptr <- function(x, ...) {
  opts <- .cstr_fetch_opts("externalptr", ...)
  if (is_corrupted_externalptr (x)) return(NextMethod())
  constructor <- constructors$externalptr [[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_externalptr <- function(x) {
  typeof(x) != "externalptr"
}

constructors$externalptr$default <- function(x, ...) {
  code <- sprintf('constructive::.xptr("%s")', external_pointer_address(x))
  repair_attributes_externalptr(x, code, ...)
}

repair_attributes_externalptr <- function(x, code, ...) {
  code
}

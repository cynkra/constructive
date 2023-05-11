constructors$externalptr <- new.env()

#' Constructive options fofr type 'externalptr'
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
#' @param origin Origin to be used, ignored when irrelevant.
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_externalptr  <- function(constructor = c("default"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  constructive_options("externalptr", constructor = constructor)
}

#' @export
construct_raw.externalptr <- function(x, ...) {
  opts <- fetch_opts("externalptr", ...)
  if (is_corrupted_externalptr (x)) return(NextMethod())
  constructor <- constructors$externalptr [[opts$constructor]]
  constructor(x, ...)
}

#' @export
is_corrupted_externalptr <- function(x) {
  typeof(x) != "externalptr"
}

constructors$externalptr$default <- function(x, ...) {
  code <- sprintf('constructive::external_pointer("%s")', external_pointer_address(x))
  repair_attributes.externalptr(x, code, ...)
}

#' @export
repair_attributes.externalptr <- function(x, code, ...) {
  code
}


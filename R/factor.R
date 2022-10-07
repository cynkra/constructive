#' Constructive options for class 'factor'
#'
#' These options will be used on objects of class 'factor'.
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"factor"` (default): Build the object using a `factor()` call, levels won't
#'   be defined explicitly if they are in alphabetical order (locale dependent!)
#' * `"as_factor"` : Build the object using a `forcats::as_factor()` call whenever
#'   possible, levels won't be defined explicitly if they are in the order of appearance.
#'   back to `data.frame()`.
#' * `"new_factor"` : Build the object using a `vctrs::new_factor()` call. Levels are
#'   always defined explicitly.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#'
#' @return An object of class <constructive_options/constructive_options_factor>
#' @export
opts_factor <- function(constructor = c("factor", "as_factor", "new_factor")) {
  constructor <- rlang::arg_match(constructor)
  structure(
    class = c("constructive_options", "constructive_options_factor"),
    list(constructor = constructor)
  )
}

#' @export
construct_idiomatic.factor <- function(x, ...) {
  args <- fetch_opts("factor", ...)
  constructor <- args$constructor
  levs <- levels(x)

  if (constructor == "new_factor") {
    code <- construct_apply(list(as.integer(x), levels = levs), "vctrs::new_factor", ...)
    return(code)
  }

  if (constructor == "as_factor" && identical(unique(as.character(x)), levs)) {
    code <- construct_apply(list(levs[x]), "forcats::as_factor", new_line =  FALSE, ...)
    return(code)
  }

  # constructor == "factor"
  default_levs <- sort(unique(as.character(x)))
  if (identical(default_levs, levs)) {
    code <- construct_apply(list(levs[x]), "factor", new_line =  FALSE, ...)
  } else {
    code <- construct_apply(list(levs[x], levels = levs), "factor", ...)
  }
  code
}

#' @export
repair_attributes.factor <- function(x, code, ..., pipe = "base") {
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = "levels",
    idiomatic_class = "factor"
  )
}

#' Constructive options for class 'ordered'
#'
#' These options will be used on objects of class 'ordered'.
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"ordered"` (default): Build the object using a `ordered()` call, levels won't
#'   be defined explicitly if they are in alphabetical order (locale dependent!)
#' * `"factor"` : Same as abvove but build the object using a `factor()` call and `ordered = TRUE`.
#' * `"new_ordered"` : Build the object using a `vctrs::new_ordered()`. Levels are
#'   always defined explicitly.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#'
#' @return An object of class <constructive_options/constructive_options_factor>
#' @export
opts_ordered <- function(constructor = c("ordered", "factor", "new_ordered"), ...) {
  combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  structure(
    class = c("constructive_options", "constructive_options_ordered"),
    list(constructor = constructor)
  )
}

#' @export
construct_idiomatic.ordered <- function(x, ...) {
  opts <- fetch_opts("ordered", ...)
  constructor <- opts$constructor
  levs <- levels(x)

  if (constructor == "new_ordered") {
    code <- construct_apply(list(as.integer(x), levels = levs), "vctrs::new_ordered", ...)
    return(code)
  }

  args <- list(levs[x])
  default_levs <- sort(unique(as.character(x)))
  if (identical(default_levs, levs)) {
    if (constructor == "factor") args <- c(args, list(ordered = TRUE))
    code <- construct_apply(args, constructor, new_line =  constructor == "factor", ...)
  } else {
    args <- c(args, list(levels = levs))
    if (constructor == "factor") args <- c(args, list(ordered = TRUE))
    code <- construct_apply(args, constructor, ...)
  }
  code
}

#' @export
repair_attributes.ordered <- function(x, code, ..., pipe = "base") {
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = "levels",
    idiomatic_class = c("ordered", "factor")
  )
}

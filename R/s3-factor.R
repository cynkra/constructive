constructors$factor <- new.env()

#' Constructive options for class 'factor'
#'
#' These options will be used on objects of class 'factor'.
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"factor"` (default): Build the object using a `factor()` call, levels won't
#'   be defined explicitly if they are in alphabetical order (locale dependent!)
#' * `"as_factor"` : Build the object using a `forcats::as_factor()` call whenever
#'   possible, i.e. when levels are defined in order of appearance in the vector. Otherwise falls back to `"factor"` constructor.
#' * `"new_factor"` : Build the object using a `vctrs::new_factor()` call. Levels are
#'   always defined explicitly.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"atomic"` : We define as an atomic vector and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_factor>
#' @export
opts_factor <- function(constructor = c("factor", "as_factor", "new_factor", "next", "atomic"), ...) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "factor"),
    ellipsis::check_dots_empty()
  )
  .cstr_options("factor", constructor = constructor)
}

#' @export
.cstr_construct.factor <- function(x, ...) {
  opts <- .cstr_fetch_opts("factor", ...)
  if (is_corrupted_factor(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$factor[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_factor <- function(x) {
  # FIXME
  typeof(x) != "integer"
}

constructors$factor$atomic <- function(x, ...) {
  .cstr_construct.atomic(x, ...)
}

constructors$factor$new_factor <- function(x, ...) {
  levs <- levels(x)
  code <- .cstr_apply(list(setNames(as.integer(x), names(x)), levels = levs), "vctrs::new_factor", ...)
  repair_attributes_factor(x, code, ...)
}

constructors$factor$as_factor <- function(x, ...) {
  levs <- levels(x)
  x_chr <- as.character(x)
  if (!identical(unique(x_chr), levs)) return(constructors$factor$factor(x, ...))
  x_chr_named <- setNames(x_chr, names(x))
  code <- .cstr_apply(list(x_chr_named), "forcats::as_factor", new_line =  FALSE, ...)
  repair_attributes_factor(x, code, ...)
}

constructors$factor$factor <- function(x, ...) {
  levs <- levels(x)
  x_chr <- as.character(x)
  x_chr_named <- setNames(x_chr, names(x))
  default_levs <- sort(unique(x_chr))
  if (identical(default_levs, levs)) {
    code <- .cstr_apply(list(x_chr_named), "factor", new_line =  FALSE, ...)
  } else {
    code <- .cstr_apply(list(x_chr_named, levels = levs), "factor", ...)
  }
  repair_attributes_factor(x, code, ...)
}

repair_attributes_factor <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = "levels",
    idiomatic_class = "factor"
  )
}

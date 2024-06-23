#' Constructive options for class 'factor'
#'
#' These options will be used on objects of class 'factor'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"factor"` (default): Build the object using `factor()`, levels won't
#'   be defined explicitly if they are in alphabetical order (locale dependent!)
#' * `"as_factor"` : Build the object using `forcats::as_factor()` whenever
#'   possible, i.e. when levels are defined in order of appearance in the vector. Otherwise falls back to `"factor"` constructor.
#' * `"new_factor"` : Build the object using `vctrs::new_factor()`. Levels are
#'   always defined explicitly.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"integer"` : We define as an integer vector and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_factor>
#' @export
opts_factor <- function(constructor = c("factor", "as_factor", "new_factor", "next", "integer"), ...) {
  .cstr_options("factor", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct factor
.cstr_construct.factor <- function(x, ...) {
  opts <- list(...)$opts$factor %||% opts_factor()
  if (is_corrupted_factor(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.factor", structure(NA_integer_, class = opts$constructor))
}

is_corrupted_factor <- function(x) {
  # FIXME
  typeof(x) != "integer"
}

#' @export
#' @method .cstr_construct.factor integer
.cstr_construct.factor.integer <- function(x, ...) {
  .cstr_construct.integer(x, ...)
}

#' @export
#' @method .cstr_construct.factor new_factor
.cstr_construct.factor.new_factor <- function(x, ...) {
  levs <- levels(x)
  code <- .cstr_apply(list(setNames(as.integer(x), names(x)), levels = levs), "vctrs::new_factor", ...)
  repair_attributes_factor(x, code, ...)
}

#' @export
#' @method .cstr_construct.factor as_factor
.cstr_construct.factor.as_factor <- function(x, ...) {
  levs <- levels(x)
  x_chr <- as.character(x)
  if (!identical(unique(x_chr), levs) || NA %in% levs) return(.cstr_construct.factor.factor(x, ...))
  x_chr_named <- setNames(x_chr, names(x))
  code <- .cstr_apply(list(x_chr_named), "forcats::as_factor", new_line =  FALSE, ...)
  repair_attributes_factor(x, code, ...)
}

#' @export
#' @method .cstr_construct.factor factor
.cstr_construct.factor.factor <- function(x, ...) {
  levs <- levels(x)
  x_chr <- as.character(x)
  x_chr_named <- setNames(x_chr, names(x))
  default_levs <- sort(unique(x_chr))
  args <- list(x_chr_named)
  if (!identical(default_levs, levs)) args$levels <- levs
  if (NA %in% levs) args["exclude"] <- list(NULL)


  if (length(args) == 1) {
    code <- .cstr_apply(args, "factor", new_line =  FALSE, ...)
  } else {
    code <- .cstr_apply(args, "factor", ...)
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

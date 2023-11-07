constructors$ordered <- new.env()

#' Constructive options for class 'ordered'
#'
#' These options will be used on objects of class 'ordered'.
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"ordered"` (default): Build the object using `ordered()`, levels won't
#'   be defined explicitly if they are in alphabetical order (locale dependent!)
#' * `"factor"` : Same as above but build the object using `factor()` and `ordered = TRUE`.
#' * `"new_ordered"` : Build the object using `vctrs::new_ordered()`. Levels are
#'   always defined explicitly.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"atomic"` : We define as an atomic vector and repair attributes
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_factor>
#' @export
opts_ordered <- function(constructor = c("ordered", "factor", "new_ordered", "next", "atomic"), ...) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "ordered"),
    ellipsis::check_dots_empty()
  )
  .cstr_options("ordered", constructor = constructor)
}

#' @export
.cstr_construct.ordered <- function(x, ...) {
  opts <- .cstr_fetch_opts("ordered", ...)
  if (is_corrupted_ordered(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$ordered[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_ordered <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$ordered$ordered <- function(x, ...) {
  levs <- levels(x)
  args <- list(setNames(as.character(x), names(x)))
  default_levs <- sort(unique(as.character(x)))
  if (!identical(default_levs, levs)) args$levels <- levs
  if (NA %in% levs) args["exclude"] <- list(NULL)
  if (length(args) == 1) {
    code <- .cstr_apply(args, "ordered", new_line =  FALSE, ...)
  } else {
    code <- .cstr_apply(args, "ordered", ...)
  }
  repair_attributes_ordered(x, code, ...)
}

#' @export
constructors$ordered$factor <- function(x, ...) {
  levs <- levels(x)
  args <- list(setNames(as.character(x), names(x)))
  default_levs <- sort(unique(as.character(x)))
  if (!identical(default_levs, levs)) args$levels <- levs
  if (NA %in% levs) args["exclude"] <- list(NULL)
  args$ordered <- TRUE
  code <- .cstr_apply(args, "factor", ...)
  repair_attributes_ordered(x, code, ...)
}


#' @export
constructors$ordered$new_ordered <- function(x, ...) {
  levs <- levels(x)
  code <- .cstr_apply(list(setNames(as.integer(x), names(x)), levels = levs), "vctrs::new_ordered", ...)
  repair_attributes_ordered(x, code, ...)
}

#' @export
constructors$ordered$atomic <- function(x, ...) {
  .cstr_construct.atomic(x, ...)
}

repair_attributes_ordered <- function(x, code, ..., pipe = NULL) {
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = "levels",
    idiomatic_class = c("ordered", "factor")
  )
}

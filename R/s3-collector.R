#' Constructive options for class 'collector'
#'
#' These options will be used on objects of class 'collector', i.e. column
#' specifications as created by `readr::col_double()`, `readr::col_date()`,
#' `readr::col_factor()` etc. They all share the class 'collector' and a subclass
#' 'collector_<type>' that maps to the constructor `readr::col_<type>()`.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"col"` (default): We build the object using the relevant `readr::col_*()`
#'   function, e.g. `readr::col_double()` or `readr::col_date(format = "%Y")`,
#'   providing only non default arguments.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `"list"`
#' * `"list"` : We define as a list and repair attributes
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_collector>
#' @export
opts_collector <- function(constructor = c("col", "next", "list"), ...) {
  .cstr_options("collector", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct collector
.cstr_construct.collector <- function(x, ...) {
  opts <- list(...)$opts$collector %||% opts_collector()
  if (is_corrupted_collector(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.collector", structure(NA, class = opts$constructor))
}

# arguments of the `readr::col_*()` functions, which are stored as is in the object
collector_args <- list(
  collector_character = NULL,
  collector_date = "format",
  collector_datetime = "format",
  collector_double = NULL,
  collector_factor = c("levels", "ordered", "include_na"),
  collector_guess = NULL,
  collector_integer = NULL,
  collector_logical = NULL,
  collector_number = NULL,
  collector_skip = NULL,
  collector_time = "format"
)

is_corrupted_collector <- function(x) {
  cl <- class(x)
  if (!is.list(x) || length(cl) != 2 || cl[[2]] != "collector") return(TRUE)
  if (!cl[[1]] %in% names(collector_args)) return(TRUE)
  args <- collector_args[[cl[[1]]]]
  if (length(x) != length(args) || !identical(names(x), args)) return(TRUE)
  cl[[1]] == "collector_factor" && !is.null(x$levels) && !is.character(x$levels)
}

#' @export
#' @method .cstr_construct.collector col
.cstr_construct.collector.col <- function(x, ...) {
  fun <- sub("^collector_", "col_", class(x)[[1]])
  args <- keep_only_non_defaults(strip(x), getExportedValue("readr", fun))
  code <- .cstr_apply(args, paste0("readr::", fun), ...)
  repair_attributes_collector(x, code, ...)
}

#' @export
#' @method .cstr_construct.collector list
.cstr_construct.collector.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_collector <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = class(x)
  )
}

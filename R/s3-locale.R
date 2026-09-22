#' Constructive options for class 'locale'
#'
#' These options will be used on objects of class 'locale'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"locale"` (default): We build the object using `readr::locale()`,
#'   providing only non default arguments. `date_names` is provided as a language
#'   code when possible.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `"list"`
#' * `"list"` : We define as a list and repair attributes
#'
#' Use `opts_date_names()` to tweak the construction of the `date_names` element,
#' `opts_date_names("date_names")` will prevent the use of a language code.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_locale>
#' @export
opts_locale <- function(constructor = c("locale", "next", "list"), ...) {
  .cstr_options("locale", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct locale
.cstr_construct.locale <- function(x, ...) {
  opts <- list(...)$opts$locale %||% opts_locale()
  if (is_corrupted_locale(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.locale", structure(NA, class = opts$constructor))
}

is_corrupted_locale <- function(x) {
  nms <- c("date_names", "date_format", "time_format", "decimal_mark", "grouping_mark", "tz", "encoding")
  if (!is.list(x) || !identical(names(x), nms)) return(TRUE)
  if (!inherits(x$date_names, "date_names")) return(TRUE)
  if (!all(vapply(x[-1], rlang::is_string, logical(1)))) return(TRUE)
  !x$decimal_mark %in% c(".", ",") || x$decimal_mark == x$grouping_mark || x$tz == ""
}

#' @export
#' @method .cstr_construct.locale locale
.cstr_construct.locale.locale <- function(x, ...) {
  args <- strip(x)
  date_names_constructor <- (list(...)$opts$date_names %||% opts_date_names())$constructor
  if (date_names_constructor == "date_names_lang") {
    args$date_names <- date_names_lang_match(x$date_names) %||% x$date_names
  }
  args <- keep_only_non_defaults(args, readr::locale)
  # `readr::locale()` infers `grouping_mark` from a non default `decimal_mark`
  implied_grouping_mark <- if (x$decimal_mark == ".") "," else "."
  if (x$grouping_mark == implied_grouping_mark) args$grouping_mark <- NULL
  code <- .cstr_apply(args, "readr::locale", ...)
  repair_attributes_locale(x, code, ...)
}

#' @export
#' @method .cstr_construct.locale list
.cstr_construct.locale.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_locale <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "locale"
  )
}

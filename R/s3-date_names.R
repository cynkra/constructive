#' Constructive options for class 'date_names'
#'
#' These options will be used on objects of class 'date_names'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"date_names_lang"` (default): We build the object using
#'   `readr::date_names_lang()` if the object matches one of the languages
#'   listed by `readr::date_names_langs()`, and fall back to the `"date_names"`
#'   constructor otherwise.
#' * `"date_names"` : We build the object using `readr::date_names()`, providing
#'   only non default arguments.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `"list"`
#' * `"list"` : We define as a list and repair attributes
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_date_names>
#' @export
opts_date_names <- function(constructor = c("date_names_lang", "date_names", "next", "list"), ...) {
  .cstr_options("date_names", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct date_names
.cstr_construct.date_names <- function(x, ...) {
  opts <- list(...)$opts$date_names %||% opts_date_names()
  if (is_corrupted_date_names(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.date_names", structure(NA, class = opts$constructor))
}

is_corrupted_date_names <- function(x) {
  if (!is.list(x) || !identical(names(x), c("mon", "mon_ab", "day", "day_ab", "am_pm"))) return(TRUE)
  if (!all(vapply(x, is.character, logical(1)))) return(TRUE)
  !identical(unname(lengths(x)[1:4]), c(12L, 12L, 7L, 7L))
}

#' @export
#' @method .cstr_construct.date_names date_names_lang
.cstr_construct.date_names.date_names_lang <- function(x, ...) {
  lang <- date_names_lang_match(x)
  if (is.null(lang)) return(.cstr_construct.date_names.date_names(x, ...))
  code <- .cstr_apply(list(lang), "readr::date_names_lang", ...)
  repair_attributes_date_names(x, code, ...)
}

#' @export
#' @method .cstr_construct.date_names date_names
.cstr_construct.date_names.date_names <- function(x, ...) {
  args <- strip(x)
  if (identical(args$mon_ab, args$mon)) args$mon_ab <- NULL
  if (identical(args$day_ab, args$day)) args$day_ab <- NULL
  if (identical(args$am_pm, c("AM", "PM"))) args$am_pm <- NULL
  code <- .cstr_apply(args, "readr::date_names", ...)
  repair_attributes_date_names(x, code, ...)
}

#' @export
#' @method .cstr_construct.date_names list
.cstr_construct.date_names.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_date_names <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "date_names"
  )
}

# language code such that `readr::date_names_lang(lang)` reproduces x, or NULL
date_names_lang_match <- function(x) {
  Find(
    function(lang) identical(readr::date_names_lang(lang), x),
    readr::date_names_langs()
  )
}

#' Constructive options for class 'vctrs_rcrd'
#'
#' These options will be used on objects of class 'vctrs_rcrd'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"new_rcrd"` (default): Use `vctrs::new_rcrd()` on the list of fields,
#'   additional attributes are passed through `...` and subclasses through the
#'   `class` argument, so this constructor also works for classes built on top
#'   of 'vctrs_rcrd'.
#' * `"next"` : Use the constructor for the next supported class.
#' * `"list"` : Use `list()` and treat the class as a regular attribute.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_vctrs_rcrd>
#' @export
opts_vctrs_rcrd <- function(constructor = c("new_rcrd", "next", "list"), ...) {
  .cstr_options("vctrs_rcrd", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct vctrs_rcrd
.cstr_construct.vctrs_rcrd <- function(x, ...) {
  opts <- list(...)$opts$vctrs_rcrd %||% opts_vctrs_rcrd()
  if (is_corrupted_vctrs_rcrd(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.vctrs_rcrd", structure(NA, class = opts$constructor))
}

is_corrupted_vctrs_rcrd <- function(x) {
  if (typeof(x) != "list" || !length(x)) return(TRUE)
  if (!identical(tail(attr(x, "class"), 2), c("vctrs_rcrd", "vctrs_vctr"))) return(TRUE)
  fields <- strip(x)
  # `vctrs::new_rcrd()` checks and normalizes fields with `vctrs::df_list()`
  normalized_fields <- tryCatch(
    vctrs::df_list(!!!fields),
    error = function(e) NULL
  )
  !identical(normalized_fields, fields)
}

#' @export
#' @method .cstr_construct.vctrs_rcrd new_rcrd
.cstr_construct.vctrs_rcrd.new_rcrd <- function(x, ...) {
  subclass <- head(attr(x, "class"), -2)
  args <- c(list(strip(x)), attributes(x)[vctrs_rcrd_dots_attr_names(x)])
  args$class <- if (length(subclass)) subclass
  code <- .cstr_apply(args, "vctrs::new_rcrd", ...)
  repair_attributes_vctrs_rcrd(x, code, ...)
}

#' @export
#' @method .cstr_construct.vctrs_rcrd list
.cstr_construct.vctrs_rcrd.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

# attributes that we can provide through `...`, others (partially matching
# `fields`) are repaired after the call
vctrs_rcrd_dots_attr_names <- function(x) {
  nms <- setdiff(names(attributes(x)), c("names", "class"))
  nms[!startsWith("fields", nms)]
}

repair_attributes_vctrs_rcrd <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = attr(x, "class"),
    ignore = vctrs_rcrd_dots_attr_names(x)
  )
}

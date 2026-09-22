#' Constructive options for class 'haven_labelled_spss'
#'
#' These options will be used on objects of class 'haven_labelled_spss'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"labelled_spss"` (default): We build the object using
#'   `haven::labelled_spss()`, the `labels`, `na_values`, `na_range` and `label`
#'   arguments are omitted when `NULL`.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_haven_labelled_spss>
#' @export
opts_haven_labelled_spss <- function(constructor = c("labelled_spss", "next"), ...) {
  .cstr_options("haven_labelled_spss", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct haven_labelled_spss
.cstr_construct.haven_labelled_spss <- function(x, ...) {
  opts <- list(...)$opts$haven_labelled_spss %||% opts_haven_labelled_spss()
  if (is_corrupted_haven_labelled_spss(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.haven_labelled_spss", structure(NA, class = opts$constructor))
}

is_corrupted_haven_labelled_spss <- function(x) {
  if (is_corrupted_haven_labelled(x)) return(TRUE)
  na_values <- attr(x, "na_values")
  if (!is.null(na_values)) {
    if (typeof(na_values) != typeof(x) || anyNA(na_values)) return(TRUE)
    if (!all(names(attributes(na_values)) %in% "names")) return(TRUE)
  }
  na_range <- attr(x, "na_range")
  if (!is.null(na_range)) {
    type_ok <- (is.character(x) && is.character(na_range)) ||
      (is.numeric(x) && is.numeric(na_range))
    if (!type_ok || length(na_range) != 2 || !is.null(attributes(na_range))) return(TRUE)
    if (anyNA(na_range) || na_range[[1]] >= na_range[[2]]) return(TRUE)
  }
  FALSE
}

#' @export
#' @method .cstr_construct.haven_labelled_spss labelled_spss
.cstr_construct.haven_labelled_spss.labelled_spss <- function(x, ...) {
  x_data <- x
  attributes(x_data) <- attributes(x)["names"]
  args <- list(
    x_data,
    labels = attr(x, "labels"),
    na_values = attr(x, "na_values"),
    na_range = attr(x, "na_range"),
    label = attr(x, "label", exact = TRUE)
  )
  args <- Filter(Negate(is.null), args)
  code <- .cstr_apply(args, "haven::labelled_spss", ...)
  repair_attributes_haven_labelled_spss(x, code, ...)
}

repair_attributes_haven_labelled_spss <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", typeof(x)),
    ignore = c("labels", "na_values", "na_range", "label")
  )
}

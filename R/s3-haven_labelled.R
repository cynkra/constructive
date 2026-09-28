#' Constructive options for class 'haven_labelled'
#'
#' These options will be used on objects of class 'haven_labelled'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"labelled"` (default): We build the object using `haven::labelled()`,
#'   the `labels` and `label` arguments are omitted when `NULL`.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_haven_labelled>
#' @export
opts_haven_labelled <- function(constructor = c("labelled", "next"), ...) {
  .cstr_options("haven_labelled", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct haven_labelled
.cstr_construct.haven_labelled <- function(x, ...) {
  opts <- list(...)$opts$haven_labelled %||% opts_haven_labelled()
  if (is_corrupted_haven_labelled(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.haven_labelled", structure(NA, class = opts$constructor))
}

is_corrupted_haven_labelled <- function(x) {
  if (!typeof(x) %in% c("double", "integer", "character")) return(TRUE)
  labels <- attr(x, "labels")
  if (!is.null(labels)) {
    if (typeof(labels) != typeof(x)) return(TRUE)
    if (!identical(names(attributes(labels)), "names")) return(TRUE)
    if (anyDuplicated(labels[!is.na(labels)])) return(TRUE)
  }
  label <- attr(x, "label", exact = TRUE)
  !is.null(label) && !(is.character(label) && length(label) == 1)
}

#' @export
#' @method .cstr_construct.haven_labelled labelled
.cstr_construct.haven_labelled.labelled <- function(x, ...) {
  x_data <- x
  attributes(x_data) <- attributes(x)["names"]
  args <- list(
    x_data,
    labels = attr(x, "labels"),
    label = attr(x, "label", exact = TRUE)
  )
  args <- Filter(Negate(is.null), args)
  code <- .cstr_apply(args, "haven::labelled", ...)
  repair_attributes_haven_labelled(x, code, ...)
}

repair_attributes_haven_labelled <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("haven_labelled", "vctrs_vctr", typeof(x)),
    ignore = c("labels", "label")
  )
}

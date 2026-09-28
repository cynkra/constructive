#' Constructive options for class 'vctrs_vctr'
#'
#' These options will be used on objects of class 'vctrs_vctr'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"new_vctr"` (default): Use `vctrs::new_vctr()` on the underlying vector,
#'   additional attributes are passed through `...`, subclasses through the
#'   `class` argument and `inherit_base_type` is set when needed, so this
#'   constructor also works for classes built on top of 'vctrs_vctr'.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_vctrs_vctr>
#' @export
opts_vctrs_vctr <- function(constructor = c("new_vctr", "next"), ...) {
  .cstr_options("vctrs_vctr", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct vctrs_vctr
.cstr_construct.vctrs_vctr <- function(x, ...) {
  opts <- list(...)$opts$vctrs_vctr %||% opts_vctrs_vctr()
  if (is_corrupted_vctrs_vctr(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.vctrs_vctr", structure(NA, class = opts$constructor))
}

is_corrupted_vctrs_vctr <- function(x) {
  type <- typeof(x)
  vector_types <- c("logical", "integer", "double", "complex", "character", "raw", "list")
  if (!type %in% vector_types) return(TRUE)
  cl <- attr(x, "class")
  # lists must inherit from their base type
  inherits_base_type <- identical(tail(cl, 2), c("vctrs_vctr", type))
  if (!inherits_base_type && (type == "list" || tail(cl, 1) != "vctrs_vctr")) return(TRUE)
  # `vctrs::new_vctr()` replaces `NA` names with `""`
  anyNA(names(x))
}

#' @export
#' @method .cstr_construct.vctrs_vctr new_vctr
.cstr_construct.vctrs_vctr.new_vctr <- function(x, ...) {
  cl <- attr(x, "class")
  inherits_base_type <- tail(cl, 1) != "vctrs_vctr"
  subclass <- head(cl, if (inherits_base_type) -2 else -1)
  args <- c(list(strip(x)), attributes(x)[vctrs_vctr_dots_attr_names(x)])
  args$class <- if (length(subclass)) subclass
  # `inherit_base_type = TRUE` is the default for lists
  if (inherits_base_type && typeof(x) != "list") args$inherit_base_type <- TRUE
  code <- .cstr_apply(args, "vctrs::new_vctr", ...)
  repair_attributes_vctrs_vctr(x, code, ...)
}

# attributes that we can provide through `...`, others (partially matching
# `.data`, or named `inherit_base_type`) are repaired after the call
vctrs_vctr_dots_attr_names <- function(x) {
  nms <- setdiff(names(attributes(x)), c("names", "class", "inherit_base_type"))
  nms[!startsWith(".data", nms)]
}

repair_attributes_vctrs_vctr <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = attr(x, "class"),
    ignore = vctrs_vctr_dots_attr_names(x)
  )
}

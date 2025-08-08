#' Constructive options for class 'ellmer::TypeArray`'
#'
#' These options will be used on objects of class 'ellmer::TypeArray`'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"TypeArray"` (default): We build the object using `ellmer::TypeArray()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_ellmer::TypeArray`>
#' @export
opts_ellmer_TypeArray <- function(constructor = c("type_array", "TypeArray", "next"), ...) {
  constructive::.cstr_options("ellmer::TypeArray", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
`.cstr_construct.ellmer::TypeArray` <- function(x, ...) {
  opts <- list(...)$opts$`ellmer::TypeArray` %||% opts_ellmer_TypeArray()
  if (`is_corrupted_ellmer::TypeArray`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ellmer::TypeArray", structure(NA, class = opts$constructor))
}

`is_corrupted_ellmer::TypeArray` <- function(x) {
  FALSE
}

#' @export
`.cstr_construct.ellmer::TypeArray.type_array` <- function(x, ...) {
  args <- as.list(c(
    attr(x, "description"),
    list(items = attr(x, "items")),
    required = attr(x, "required")
  ))
  args <- keep_only_non_defaults(args, ellmer::type_array)
  code <- constructive::.cstr_apply(args, fun = "ellmer::type_array", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("ellmer::TypeArray", "ellmer::Type", "S7_object"),
    ignore = c("S7_class", "description", "required", "items")
  )
}

#' @export
`.cstr_construct.ellmer::TypeArray.TypeArray` <- function(x, ...) {
  args <- list(
    description = attr(x, "description"),
    items = attr(x, "items"),
    required = attr(x, "required")
  )
  args <- keep_only_non_defaults(args, ellmer::TypeArray)
  names(args)[names(args) == "description"] <- ""
  code <- constructive::.cstr_apply(args, fun = "ellmer::TypeArray", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("ellmer::TypeArray", "ellmer::Type", "S7_object"),
    ignore = c("S7_class", "description", "required", "items")
  )
}

#' Constructive options for class 'ellmer::TypeEnum`'
#'
#' These options will be used on objects of class 'ellmer::TypeEnum`'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"TypeEnum"` (default): We build the object using `ellmer::TypeEnum()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_ellmer::TypeEnum`>
#' @export
opts_ellmer_TypeEnum <- function(constructor = c("type_enum", "TypeEnum", "next"), ...) {
  constructive::.cstr_options("ellmer::TypeEnum", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
`.cstr_construct.ellmer::TypeEnum` <- function(x, ...) {
  opts <- list(...)$opts$`ellmer::TypeEnum` %||% opts_ellmer_TypeEnum()
  if (`is_corrupted_ellmer::TypeEnum`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ellmer::TypeEnum", structure(NA, class = opts$constructor))
}

`is_corrupted_ellmer::TypeEnum` <- function(x) {
  FALSE
}

#' @export
`.cstr_construct.ellmer::TypeEnum.type_enum` <- function(x, ...) {
  args <- list(
    description = attr(x, "description"),
    values = attr(x, "values"),
    required = attr(x, "required")
  )
  args <- keep_only_non_defaults(args, ellmer::type_enum)
  names(args)[names(args) == "description"] <- ""
  code <- constructive::.cstr_apply(args, fun = "ellmer::type_enum", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("ellmer::TypeEnum", "ellmer::Type", "S7_object"),
    ignore = c("S7_class", "description", "required", "values")
  )
}

#' @export
`.cstr_construct.ellmer::TypeEnum.TypeEnum` <- function(x, ...) {
  args <- list(
    description = attr(x, "description"),
    values = attr(x, "values"),
    required = attr(x, "required")
  )
  args <- keep_only_non_defaults(args, ellmer::type_enum)
  names(args)[names(args) == "description"] <- ""
  code <- constructive::.cstr_apply(args, fun = "ellmer::TypeEnum", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("ellmer::TypeEnum", "ellmer::Type", "S7_object"),
    ignore = c("S7_class", "description", "required", "values")
  )
}

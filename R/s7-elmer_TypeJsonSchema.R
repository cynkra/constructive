#' Constructive options for class 'ellmer::TypeJsonSchema`'
#'
#' These options will be used on objects of class 'ellmer::TypeJsonSchema`'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"TypeJsonSchema"` (default): We build the object using `ellmer::TypeJsonSchema()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_ellmer::TypeJsonSchema`>
#' @export
opts_ellmer_TypeJsonSchema <- function(constructor = c("type_from_schema", "TypeJsonSchema", "next"), ...) {
  constructive::.cstr_options("ellmer::TypeJsonSchema", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
`.cstr_construct.ellmer::TypeJsonSchema` <- function(x, ...) {
  opts <- list(...)$opts$`ellmer::TypeJsonSchema` %||% opts_ellmer_TypeJsonSchema()
  if (`is_corrupted_ellmer::TypeJsonSchema`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ellmer::TypeJsonSchema", structure(NA, class = opts$constructor))
}

`is_corrupted_ellmer::TypeJsonSchema` <- function(x) {
  FALSE
}

#' @export
`.cstr_construct.ellmer::TypeJsonSchema.type_from_schema` <- function(x, ...) {
  args <- list(unclass(jsonlite::toJSON(attr(x, "json"), auto_unbox = TRUE)))
  args <- keep_only_non_defaults(args, ellmer::type_from_schema)
  code <- constructive::.cstr_apply(args, fun = "ellmer::type_from_schema", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("ellmer::TypeJsonSchema", "ellmer::Type", "S7_object"),
    ignore = c("S7_class", "json", "required")
  )
}

#' @export
`.cstr_construct.ellmer::TypeJsonSchema.TypeJsonSchema` <- function(x, ...) {
  args <- list(
    description = attr(x, "description"),
    required = attr(x, "required"),
    json = attr(x, "json")
  )
  args <- keep_only_non_defaults(args, ellmer::TypeJsonSchema)
  names(args)[names(args) == "description"] <- ""
  code <- constructive::.cstr_apply(args, fun = "ellmer::TypeJsonSchema", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("ellmer::TypeJsonSchema", "ellmer::Type", "S7_object"),
    ignore = c("S7_class", "description", "required", "json")
  )
}

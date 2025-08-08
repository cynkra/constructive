#' Constructive options for class 'ellmer::TypeBasic`'
#'
#' These options will be used on objects of class 'ellmer::TypeBasic`'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"TypeBasic"` (default): We build the object using `ellmer::TypeBasic()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_ellmer::TypeBasic`>
#' @export
opts_ellmer_TypeBasic <- function(constructor = c("default", "TypeBasic", "next"), ...) {
  constructive::.cstr_options("ellmer::TypeBasic", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
`.cstr_construct.ellmer::TypeBasic` <- function(x, ...) {
  opts <- list(...)$opts$`ellmer::TypeBasic` %||% opts_ellmer_TypeBasic()
  if (`is_corrupted_ellmer::TypeBasic`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ellmer::TypeBasic", structure(NA, class = opts$constructor))
}

`is_corrupted_ellmer::TypeBasic` <- function(x) {
  FALSE
}

#' @export
`.cstr_construct.ellmer::TypeBasic.default` <- function(x, ...) {
  fun0 <- paste0("type_", attr(x, "type"))
  fun <- paste0("ellmer::", fun0)
  args <- list(
    description = attr(x, "description"),
    required = attr(x, "required")
  )
  args <- keep_only_non_defaults(args, getFromNamespace(fun0, "ellmer"))
  names(args)[names(args) == "description"] <- ""
  code <- constructive::.cstr_apply(args, fun, ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("ellmer::TypeBasic", "ellmer::Type", "S7_object"),
    ignore = c("S7_class", "description", "required", "type")
  )
}

#' @export
`.cstr_construct.ellmer::TypeBasic.TypeBasic` <- function(x, ...) {
  args <- list(
    description = attr(x, "description"),
    required = attr(x, "required"),
    type = attr(x, "type")
  )
  args <- keep_only_non_defaults(args, ellmer::TypeBasic)
  names(args)[names(args) == "description"] <- ""
  code <- constructive::.cstr_apply(args, fun = "ellmer::TypeBasic", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("ellmer::TypeBasic", "ellmer::Type", "S7_object"),
    ignore = c("S7_class", "description", "required", "type")
  )
}

#' Constructive options for class 'ellmer::TypeObject`'
#'
#' These options will be used on objects of class 'ellmer::TypeObject`'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"TypeObject"` (default): We build the object using `ellmer::TypeObject()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_ellmer::TypeObject`>
#' @export
opts_ellmer_TypeObject <- function(constructor = c("type_object", "TypeObject", "next"), ...) {
  constructive::.cstr_options("ellmer::TypeObject", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
`.cstr_construct.ellmer::TypeObject` <- function(x, ...) {
  opts <- list(...)$opts$`ellmer::TypeObject` %||% opts_ellmer_TypeObject()
  if (`is_corrupted_ellmer::TypeObject`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ellmer::TypeObject", structure(NA, class = opts$constructor))
}

`is_corrupted_ellmer::TypeObject` <- function(x) {
  FALSE
}

#' @export
`.cstr_construct.ellmer::TypeObject.type_object` <- function(x, ...) {
  args <- c(
    list(.description = attr(x, "description")),
    attr(x, "properties"),
    list(.required = attr(x, "required"))
  )
  # `.additional_properties` defaults to `FALSE` for `type_object()` in all
  # ellmer versions and was deprecated in ellmer 0.5.0, so only emit it when it
  # is `TRUE`. This avoids a deprecation warning and keeps the reconstructed
  # code idiomatic. (We can't rely on `keep_only_non_defaults()` to drop it
  # because ellmer's formal default is now `deprecated()`, not `FALSE`.)
  if (isTRUE(attr(x, "additional_properties"))) {
    args$.additional_properties <- attr(x, "additional_properties")
  }
  args <- keep_only_non_defaults(args, ellmer::type_object)
  names(args)[names(args) == ".description"] <- ""
  code <- constructive::.cstr_apply(args, fun = "ellmer::type_object", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("ellmer::TypeObject", "ellmer::Type", "S7_object"),
    ignore = c("S7_class", "description", "required", "additional_properties", "properties")
  )
}

#' @export
`.cstr_construct.ellmer::TypeObject.TypeObject` <- function(x, ...) {
  args <- list(
    description = attr(x, "description"),
    required = attr(x, "required"),
    properties = attr(x, "properties"),
    additional_properties = attr(x, "additional_properties")
  )
  args <- keep_only_non_defaults(args, ellmer::TypeObject)
  names(args)[names(args) == "description"] <- ""
  code <- constructive::.cstr_apply(args, fun = "ellmer::TypeObject", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("ellmer::TypeObject", "ellmer::Type", "S7_object"),
    ignore = c("S7_class", "description", "required", "additional_properties", "properties")
  )
}

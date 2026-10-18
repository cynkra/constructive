#' Constructive options for class 'S7_object'
#'
#' These options will be used on objects of class 'S7_object'. This class is
#' unusual in that it can be applied on objects of different internal types,
#' objects of length 1 class `"S7_object"` are of type `"object"` while
#' for instance objects of class `c("S7_class", "S7_object")` are closures.
#' For closures we fall back to the next method.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"S7_object"` (default): We build the object using `S7::S7_object()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @section Before the "object" type: 
#' 
#' The information above is correct only starting from R 4.4.
#' The "object" type was introduced in R 4.4 and the S7 package uses the "S4" type
#' instead for previous versions. 
#' 
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_S7_object>
#' @export
opts_S7_object <- function(constructor = c("S7_object", "next"), ...) {
  constructive::.cstr_options("S7_object", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.S7_object <- function(x, ...) {
  opts <- list(...)$opts$S7_object %||% opts_S7_object()
  if (is_corrupted_S7_object(x) || opts$constructor == "next" || is.function(x)) return(NextMethod())
  UseMethod(".cstr_construct.S7_object", structure(NA, class = opts$constructor))
}

is_corrupted_S7_object <- function(x) {
  # many object inheriting from "S7_object" are not of type "object"
  # for instance ggplot2 themes after v3.5.2
  # here we consider those corrupted as there are no constructors to build those
  # directly anyway
  if (with_versions(R < "4.4")) {
    # see ?opts_S7_object
    typeof(x) != "S4"
  } else {
    typeof(x) != "object"
  }
  
}


#' @export
.cstr_construct.S7_object.new_class<- function(x, ...) {
  constructor <- attr(s7_class(x), "constructor")
  we_can_use_compact_syntax <-
    !identical(x, S7::S7_object()) && is_default_s7_class_constructor(constructor)
  if (!we_can_use_compact_syntax) return(.cstr_construct.S7_object.S7_object(x, ...))
  fun_code <- .cstr_construct(s7_class(x), ...)
  args <- attributes(x)
  args$class <- NULL
  args[s7_internal_attrs] <- NULL
  code <- constructive::.cstr_apply(args, fun = fun_code, ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    ignore = setdiff(names(attributes(x)), "class"),
    idiomatic_class = c(attr(s7_class(x), "name"), "S7_object")
  )
}

#' @export
.cstr_construct.S7_object.S7_object <- function(x, ...) {
  code <- "S7::S7_object()"
  # since S7 0.2.2.9000 `S7_object()` sets `_S7_version = 1L`
  version_set <-
    with_versions(S7 > "0.2.1") && identical(attr(x, "_S7_version"), 1L)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    ignore = if (version_set) "_S7_version",
    idiomatic_class = "S7_object"
  )
}


# Attributes that S7 sets itself. The class of an object is stored in the
# "S7_class" attribute up to S7 0.2.1, and since S7 0.2.2.9000 in "_S7_class"
# along with "_S7_version", generally through a shared reference to the class
s7_internal_attrs <- c("S7_class", "_S7_class", "_S7_version")

s7_class <- function(x) {
  class <- attr(x, "_S7_class") %||% attr(x, "S7_class")
  if (is.environment(class)) class <- get("class", envir = class)
  class
}

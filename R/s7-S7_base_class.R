#' Constructive options for class 'S7_base_class'
#'
#' These options will be used on objects of class 'S7_base_class'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"S7"` (default): We build the object using `S7()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_S7_base_class>
#' @export
opts_S7_base_class <- function(constructor = c("S7", "next"), ...) {
  constructive::.cstr_options("S7_base_class", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.S7_base_class <- function(x, ...) {
  opts <- list(...)$opts$S7_base_class %||% opts_S7_base_class()
  if (is_corrupted_S7_base_class(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.S7_base_class", structure(NA, class = opts$constructor))
}

is_corrupted_S7_base_class <- function(x) {
  FALSE
}

#' @export
.cstr_construct.S7_base_class.S7 <- function(x, ...) {
  # opts <- list(...)$opts$S7_base_class %||% opts_S7_base_class()
  args <- list()
  # FIXME: the following is not enough to construct ellmer types
  # Filter(\(x) inherits(x, "S7_base_class"), mget(getNamespaceExports("S7"), inherits = TRUE)) |> names() |> construct()
  # candidates <- c(
  #   "class_name", "class_double", "class_complex", "class_call", "class_raw",
  #   "class_character", "class_expression", "class_list", "class_environment",
  #   "class_integer", "class_logical", "class_function"
  # )
  # fun_nm <- perfect_match(x, mget(candidates, asNamespace("S7")))
  # code <- paste0("S7::", fun_nm)
  suffix <- x$constructor_name
  if (suffix == "fun") suffix <- "function"
  code <- paste0("S7::class_", suffix)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "S7_base_class"
  )
}

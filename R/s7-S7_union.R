#' Constructive options for class 'S7_union'
#'
#' These options will be used on objects of class 'S7_union'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"default"` : We build the object using its name if it's a "class_union"
#'   object provided by the 'S7' package, or fall back to the `"|"` constructor.
#' * `"|"` : We build the object using the `|` operator.
#' * `"new_union"` (default): We build the object using `S7::new_union()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_S7_union>
#' @export
opts_S7_union <- function(constructor = c("default", "|", "new_union", "next"), ...) {
  constructive::.cstr_options("S7_union", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.S7_union <- function(x, ...) {
  opts <- list(...)$opts$S7_union %||% opts_S7_union()
  if (is_corrupted_S7_union(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.S7_union", structure(NA, class = opts$constructor))
}

is_corrupted_S7_union <- function(x) {
  FALSE
}

#' @export
.cstr_construct.S7_union.default <- function(x, ...) {
  # opts <- list(...)$opts$S7_union %||% opts_S7_union()
  args <- list()
  # Filter(\(x) inherits(x, "S7_union"), mget(getNamespaceExports("S7"), inherits = TRUE)) |> names() |> construct()
  candidates <- c("class_numeric", "class_vector", "class_language", "class_atomic")
  fun_nm <- perfect_match(x, mget(candidates, asNamespace("S7")))
  if (!length(fun_nm)) {
    return(`.cstr_construct.S7_union.|`(x, ...))
  }
  code <- paste0("S7::", fun_nm)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "S7_union"
  )
}

#' @export
`.cstr_construct.S7_union.|` <- function(x, ...) {
  # opts <- list(...)$opts$S7_union %||% opts_S7_union()
  args <- x$classes
  arg_code <- lapply(args, .cstr_construct, ...)
  code <- paste(arg_code, collapse = " | ")
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "S7_union"
  )
}

#' @export
.cstr_construct.S7_union.new_union <- function(x, ...) {
  # opts <- list(...)$opts$S7_union %||% opts_S7_union()
  args <- x$classes
  code <- constructive::.cstr_apply(args, fun = "S7::new_union", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "S7_union"
  )
}

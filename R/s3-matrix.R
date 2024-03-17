constructors$matrix <- new.env()

#' Constructive options for matrices
#'
#' Matrices are atomic vectors, lists, or objects of type `"expression"` with a `"dim"`
#' attributes of length 2.
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"matrix"` : We use `matrix()`
#' * `"array"` : We use `array()`
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `"array"`
#' * `"atomic"` : We define as an atomic vector and repair attributes
#'
#' @param constructor String. Name of the function used to construct the environment.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_matrix  <- function(constructor = c("matrix", "array", "next", "atomic"), ...) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "matrix"),
    check_dots_empty()
  )
  .cstr_options("matrix", constructor = constructor)
}

#' @export
.cstr_construct.matrix <- function(x, ...) {
  opts <- .cstr_fetch_opts("matrix", ...)
  if (is_corrupted_matrix(x) || opts$constructor == "next") return(NextMethod())
  constructors$matrix[[opts$constructor]](x, ...)
}

is_corrupted_matrix <- function(x) {
  is_corrupted_array(x) || length(dim(x)) != 2
}

constructors$matrix$matrix <- function(x, ...) {
  dim <- attr(x, "dim")
  dimnames <- attr(x, "dimnames")
  dim_names_lst <- if (!is.null(dimnames)) list(dimnames = dimnames)
  x_stripped <- x
  attributes(x_stripped) <- NULL
  code <- .cstr_apply(
    c(list(x_stripped, nrow = dim[[1]], ncol = dim[[2]]), dim_names_lst),
    "matrix",
    ...
  )
  repair_attributes_matrix(x, code, ...)
}

constructors$matrix$array <- function(x, ...) {
  code <- constructors$array$array(x, ...)
  repair_attributes_matrix(x, code, ...)
}

constructors$matrix$atomic <- function(x, ...) {
  .cstr_construct.default(x, ...)
}

repair_attributes_matrix <- function(x, code, ..., pipe = NULL) {
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = c("dim", "dimnames")
  )
}

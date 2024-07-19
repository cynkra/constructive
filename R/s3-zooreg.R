#' Constructive options for class 'zooreg'
#'
#' These options will be used on objects of class 'zooreg'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"zooreg"` (default): We build the object using `zoo::zooreg()`, using the
#'   `start` and `frequency` arguments.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_zooreg>
#' @export
opts_zooreg <- function(constructor = c("zooreg", "next"), ...) {
  .cstr_options("zooreg", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct zooreg
.cstr_construct.zooreg <- function(x, ...) {
  opts <- list(...)$opts$zooreg %||% opts_zooreg()
  if (is_corrupted_zooreg(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.zooreg", structure(NA, class = opts$constructor))
}

is_corrupted_zooreg <- function(x) {
  is_corrupted_zoo(x)
}

#' @export
#' @method .cstr_construct.zooreg zooreg
.cstr_construct.zooreg.zooreg <- function(x, ...) {
  args <- list(
    structure(strip(x), dim = dim(x), dimnames = dimnames(x)),
    start = attr(x, "index")[[1]],
    frequency = attr(x, "frequency")
  )
  code <- .cstr_apply(args, fun = "zoo::zooreg", ...)
  .cstr_repair_attributes(
    x, code, ...,
    ignore = c("index", "frequency", "dim", "dimnames"),
    idiomatic_class = c("zooreg", "zoo")
  )
}

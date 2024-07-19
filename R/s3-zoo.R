#' Constructive options for class 'zoo'
#'
#' These options will be used on objects of class 'zoo'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"zoo"` (default): We build the object using `zoo::zoo()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_zoo>
#' @export
opts_zoo <- function(constructor = c("zoo", "next"), ...) {
  .cstr_options("zoo", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct zoo
.cstr_construct.zoo <- function(x, ...) {
  opts <- list(...)$opts$zoo %||% opts_zoo()
  if (is_corrupted_zoo(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.zoo", structure(NA, class = opts$constructor))
}

is_corrupted_zoo <- function(x) {
  if (!typeof(x) %in% c("double", "integer")) return(TRUE)
  index <- attr(x, "index")
  if (!typeof(index) %in% c("double", "integer")) return(TRUE)
  index_has_unsupported_class <-
    !is.null(attr(index, "class")) &&
    !inherits(index, "POSIXct") &&
    !inherits(index, "Date") &&
    !inherits(index, "yearmon") &&
    !inherits(index, "yearqtr")
  if (index_has_unsupported_class) return(TRUE)
  index_has_wrong_attrs <-
    length(setdiff(
      names(attributes(index)),
      c("class", if (inherits(index, "POSIXct")) "tzone")
    )) > 0
  if (index_has_wrong_attrs) return(TRUE)
  FALSE
}

#' @export
#' @method .cstr_construct.zoo zoo
.cstr_construct.zoo.zoo <- function(x, ...) {
  args <- list(
    structure(strip(x), dim = dim(x), dimnames = dimnames(x)),
    order.by = attr(x, "index")
  )
  code <- .cstr_apply(args, fun = "zoo::zoo", ...)
  .cstr_repair_attributes(
    x, code, ...,
    ignore = c("index", "dim","dimnames"),
    idiomatic_class = "zoo"
  )
}

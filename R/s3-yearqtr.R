#' Constructive options for class 'yearqtr'
#'
#' These options will be used on objects of class 'yearqtr'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as.yearqtr"` (default): We build the object using `zoo::as.yearqtr()` on
#'   a string in the format `"2000 Q3"`.
#' * `"yearqtr"` : We build the object using `zoo::yearqtr()` on a double in the
#'   format `2000.5`
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_yearqtr>
#' @export
opts_yearqtr <- function(constructor = c("as.yearqtr", "yearqtr", "next"), ...) {
  .cstr_options("yearqtr", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct yearqtr
.cstr_construct.yearqtr <- function(x, ...) {
  opts <- list(...)$opts$yearqtr %||% opts_yearqtr()
  if (is_corrupted_yearqtr(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.yearqtr", structure(NA, class = opts$constructor))
}

is_corrupted_yearqtr <- function(x) {
  !is.double(x) || any(x %% 0.25 != 0)
}

#' @export
#' @method .cstr_construct.yearqtr as.yearqtr
.cstr_construct.yearqtr.as.yearqtr <- function(x, ...) {
  # code from zoo:::format.yearqtr()
  year <- floor(unclass(x) + 0.001)
  quarter <- floor(4 * (unclass(x) - year) + 1 + 0.5 + 0.001)
  x_chr <- sprintf("%s Q%s", year, quarter)
  args <- list(x_chr)
  code <- .cstr_apply(args, fun = "zoo::as.yearqtr", ..., new_line = FALSE)
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "yearqtr"
  )
}

#' @export
#' @method .cstr_construct.yearqtr yearqtr
.cstr_construct.yearqtr.yearqtr <- function(x, ...) {
  args <- list(unclass(x))
  code <- .cstr_apply(args, fun = "zoo::yearqtr", ..., new_line = FALSE)
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "yearqtr"
  )
}

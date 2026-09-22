#' Constructive options for class 'html'
#'
#' These options will be used on objects of class 'html'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"HTML"` (default): We build the object using `htmltools::HTML()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_html>
#' @export
opts_html <- function(constructor = c("HTML", "next"), ...) {
  .cstr_options("html", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct html
.cstr_construct.html <- function(x, ...) {
  opts <- list(...)$opts$html %||% opts_html()
  if (is_corrupted_html(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.html", structure(NA, class = opts$constructor))
}

is_corrupted_html <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) return(TRUE)
  if (!identical(attr(x, "html"), TRUE)) return(TRUE)
  no_ws <- attr(x, "noWS")
  !is.null(no_ws) && !is_valid_no_ws(no_ws)
}

#' @export
#' @method .cstr_construct.html HTML
.cstr_construct.html.HTML <- function(x, ...) {
  args <- list(as.character(x))
  args$.noWS <- attr(x, "noWS")
  code <- .cstr_apply(args, fun = "htmltools::HTML", ..., new_line = FALSE)
  repair_attributes_html(x, code, ...)
}

repair_attributes_html <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("html", "character"),
    ignore = c("html", "noWS")
  )
}
